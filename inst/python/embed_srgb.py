# ------------------------------------------------------------------------------
# Declare sRGB in a figure frazil has just written, in the PNG and in the PDF.
#
# Neither export device says what its numbers mean. ragg writes 8-bit RGB with
# no color profile, and cairo_pdf writes DeviceRGB with nothing on the catalog,
# so a print shop has to be told in writing to read both files as sRGB. That
# instruction does not survive the files being forwarded to a third party, which
# is the whole reason for this script: the declaration is put inside the files,
# where it travels with them.
#
# save_slide() runs this on the two files it has just written. It is a separate
# script and not R code because the PDF needs PyMuPDF and there is no R package
# here that will edit a PDF object tree.
#
# What it adds, and what each piece is for:
#
#   PNG   an iCCP chunk holding the sRGB profile, spliced in after IHDR. The
#         compressed image data is copied byte for byte and never re-encoded, so
#         the picture cannot change. Any pHYs chunk declaring the resolution is
#         left alone.
#
#   PDF   two things, because they answer different questions. A /DefaultRGB
#         entry in each page's resources says how to interpret the DeviceRGB
#         numbers already in the file, which is the mechanism in the PDF
#         specification for exactly that (clause 8.6.5.6, default color
#         spaces). An /OutputIntents array on the catalog says what viewing
#         condition the figure was prepared for, which is what a prepress
#         preflight reports. Both point at one embedded copy of the profile.
#         The subtype is /GTS_PDFA1 because the specification offers no generic
#         subtype for an RGB intent.
#
# The profile is the operating system's own copy of the ICC sRGB profile rather
# than one vendored into the package.
#
# Run:  python embed_srgb.py <file> [<file> ...]
# Both calls are idempotent: a file that already carries the declaration is
# rewritten with the same one rather than gaining a second.
# ------------------------------------------------------------------------------

import os
import struct
import sys
import zlib

# Where each platform keeps its copy of the ICC sRGB profile.
PROFILE_PATHS = (
    r"C:\Windows\System32\spool\drivers\color\sRGB Color Space Profile.icm",
    "/System/Library/ColorSync/Profiles/sRGB Profile.icc",
    "/usr/share/color/icc/colord/sRGB.icc",
    "/usr/share/color/icc/sRGB.icc",
)
PROFILE_NAME = b"sRGB IEC61966-2.1"


def profile_bytes():
    for path in PROFILE_PATHS:
        if os.path.exists(path):
            with open(path, "rb") as fh:
                return fh.read()
    sys.exit("Found no sRGB profile. Looked in: "
             + ", ".join(PROFILE_PATHS))


def chunk(typ, data):
    return (struct.pack(">I", len(data)) + typ + data
            + struct.pack(">I", zlib.crc32(typ + data) & 0xFFFFFFFF))


def embed_png(path, icc):
    with open(path, "rb") as fh:
        raw = fh.read()
    if raw[:8] != b"\x89PNG\r\n\x1a\n":
        sys.exit("%s is not a PNG" % path)

    # Walk the chunks, keeping every one except a color declaration already
    # there. iCCP and sRGB are alternatives and the PNG specification does not
    # allow both, so any existing pair is dropped and replaced by this one.
    out, off, replaced = [raw[:8]], 8, False
    while off < len(raw):
        (ln,) = struct.unpack(">I", raw[off:off + 4])
        typ = raw[off + 4:off + 8]
        end = off + 12 + ln
        if typ in (b"iCCP", b"sRGB"):
            replaced = True
        else:
            out.append(raw[off:end])
            # The color declaration belongs in the header group, before any
            # pixel data, so it goes straight after IHDR.
            if typ == b"IHDR":
                payload = (PROFILE_NAME + b"\x00" + b"\x00"
                           + zlib.compress(icc, 9))
                out.append(chunk(b"iCCP", payload))
        off = end

    # Written beside the original and moved into place, so an interrupted run
    # cannot leave a half-written sheet where the sheet used to be.
    tmp = path + ".srgb-tmp"
    with open(tmp, "wb") as fh:
        fh.write(b"".join(out))
    os.replace(tmp, path)
    print("  PNG  iCCP %s (%d bytes of profile)%s"
          % (PROFILE_NAME.decode(), len(icc),
             ", replacing the one already there" if replaced else ""))


def embed_pdf(path, icc):
    import pymupdf

    doc = pymupdf.open(path)

    # A fresh export carries no declaration, which is the normal case, because
    # the render writes the PDF again every time. A file that already has one is
    # left exactly as it is. A PDF is written here by appending to it, so a
    # second pass would add another section and leave the first pass's copy of
    # the profile in the file with nothing pointing at it.
    if "sRGB IEC61966-2.1" in doc.xref_object(doc.pdf_catalog()):
        print("  PDF  already declares sRGB; left as it is")
        doc.close()
        return

    xref = doc.get_new_xref()
    doc.update_object(xref, "<< /N 3 /Alternate /DeviceRGB >>")
    doc.update_stream(xref, icc, new=True, compress=True)

    cat = doc.pdf_catalog()
    doc.xref_set_key(cat, "OutputIntents",
                     "[ << /Type /OutputIntent /S /GTS_PDFA1"
                     " /OutputConditionIdentifier (sRGB IEC61966-2.1)"
                     " /Info (sRGB IEC61966-2.1)"
                     " /RegistryName (http://www.color.org)"
                     " /DestOutputProfile %d 0 R >> ]" % xref)

    # Each page's own resources, because /DefaultRGB is scoped to the resource
    # dictionary and not to the document. Resources may be a dictionary written
    # inline on the page or an object of its own, and the key has to be set on
    # whichever it is.
    for page in doc:
        kind, val = doc.xref_get_key(page.xref, "Resources")
        target = int(val.split()[0]) if kind == "xref" else page.xref
        prefix = "" if kind == "xref" else "Resources/"
        if doc.xref_get_key(target, prefix + "ColorSpace")[0] == "null":
            doc.xref_set_key(target, prefix + "ColorSpace",
                             "<< /DefaultRGB [/ICCBased %d 0 R] >>" % xref)
        else:
            doc.xref_set_key(target, prefix + "ColorSpace/DefaultRGB",
                             "[/ICCBased %d 0 R]" % xref)

    # Appended to the file rather than rewritten, which leaves all 76 MB of
    # existing content streams untouched and takes a moment instead of minutes.
    doc.save(path, incremental=True, encryption=pymupdf.PDF_ENCRYPT_KEEP)
    doc.close()
    print("  PDF  output intent and /DefaultRGB on %d page(s), profile in "
          "object %d" % (1, xref))


def main(paths):
    icc = profile_bytes()
    for path in paths:
        if not os.path.exists(path):
            sys.exit("No file at %s" % path)
        print(os.path.basename(path))
        ext = os.path.splitext(path)[1].lower()
        if ext == ".png":
            embed_png(path, icc)
        elif ext == ".pdf":
            embed_pdf(path, icc)
        else:
            sys.exit("Nothing to do for %s" % path)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit(__doc__ or "Pass one or more .png or .pdf files.")
    main(sys.argv[1:])
