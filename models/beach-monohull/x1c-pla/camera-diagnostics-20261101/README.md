# X1C camera diagnostics, 2026-11-01

Read-only investigation while the authorized enlarged monohull continued printing.
No printer controls, service restarts, trust changes, or production deployments were sent.
The owner reports Bambu Studio can view the camera successfully.

## Confirmed observations

- The X1C advertises the enabled RTSPS endpoint on port 322, path
  `/streaming/live/1`, with camera present and resolution 720p.
- Independent Python TLS validation and OpenSSL s_client validation against the
  recorded printer CA and exact serial identity pass on the Mac. Certificate
  expiry is May 2035. The Mini Python TLS check also passes; its public CA-file
  SHA-256 matches the Mac's. Reachability and a valid certificate can therefore
  be demonstrated separately from successful video decoding.
- The Mac's FFmpeg 9.0.2 exits 251 in about 1-2 seconds with OpenSSL error
  `0A000086` / `certificate verify failed`, before a usable image. The precise
  difference between FFmpeg verification and the successful independent TLS
  checks remains unresolved. Do not classify this as an expired certificate,
  wrong access code, or confirmed universal TLS failure.
- The Mini's FFmpeg 9.0.2 reaches the stream but reports `Operation timed out`,
  `Failed reading RTSP data`, H.264 macroblock decoding errors (`bytestream -5`)
  and `corrupt decoded frame`. It can still exit zero and produce a valid JPEG.
- Matched diagnostic captures on the Mini: baseline returned a 20,737-byte JPEG
  after 8.8 s; strict/keyframe-only returned a 19,727-byte JPEG after 10.53 s.
  Both images were inspected and both have obvious blocks/smearing, including
  the build-plate region. The keyframe experiment did not fix image quality.
  These are diagnostic captures, not valid physical inspection of the boat.
- The production snapshot adapter suppresses decoder stderr and collapses errors
  to generic failures. It checks JPEG framing/exit status, not whether H.264
  corruption was concealed while generating that JPEG. This hides actionable
  diagnostics and can label a damaged image as a successful capture.

The current snapshot implementation creates a new stream per capture and sets
an RTSP socket timeout of 6 seconds. Timeout/read failure is observed; the exact
cause of damaged H.264 data and whether connection churn contributes have not
been established. Increasing the timeout, accepting only keyframes, or accepting
any complete JPEG are not demonstrated fixes.

## Separate printer warning

At the diagnostic checks the boat remained RUNNING around layer 19-20/320, with
`print_error=0` and HMS attr=201327360 / code=131073. That formats as
**0C00-0300-0002-0001**. Bambu Studio's installed English HMS catalog identifies
it as weak laser reflection from the filament causing exposure metering to fail;
first-layer inspection may be inaccurate. This is a laser/inspection warning,
not evidence explaining the RTSPS TLS or H.264 failures. It was not cleared.

## Interpretation and next work

Bambu Studio working supports an integration-specific explanation, but does not
establish which LAN transport or decoder it used, nor exclude a firmware issue
in the third-party streaming endpoint. A comparable report exists in Bambu's
issue tracker: https://github.com/bambulab/BambuStudio/issues/4481 . That report
is not proof of this printer having that particular firmware bug: fresh TLS
handshakes here succeed independently.

Next fixes should preserve certificate/name validation, expose bounded sanitized
error categories (TLS, authentication, timeout, decoder corruption), and evaluate
a shared persistent stream that publishes only usable frames. The production
Mini's timeout/corrupt-frame problem and the Mac's FFmpeg-specific TLS problem
must be investigated separately. No reliable camera fix has been proven or
redeployed by this diagnostic task.

Evidence: `mac-decoder.json`, `mini-decoder-comparison.json`, and the two paired
JPEGs. No access codes, private keys, signatures or cookies are included.
