## Release v0.17.0

- In the `Envelope_status` module, rename the `No_recipients` constructor to `Rejected_all_recipients`

- Add the following callbacks to `Spool.create`:
  - `presend`: Called immediately prior to sending a message. This can be used to delay a message (e.g. due to rate limiting)
  - `on_error`: Called after receiving an SMTP response. This can be used to determine how to handle the error code.
  
- Expose an optional credentials argument in the SMTP client connection pool

## Release v0.16.0

- `Mail_fingerprint.of_email` now accepts an additional `compute_md5` parameter, allowing
   users to choose whether to compute the MD5 hash while creating a fingerprint from an
   email

- Add `Simplemail.For_testing` module can be used in testing code to override the default server
  that mail is sent to

- `Envelope.of_email` and `Envelope_info.of_email` now accept an additional
  `ignore_unparseable_recipient_header` parameter that causes the function to ignore
   unparseable headers rather than returning an error

## Old pre-v0.15 changelogs (very likely stale and incomplete)

## v0.11

- Moved the `Cache` module out to its own library: `resource_cache`.
  (available at https://github.com/janestreet/resource_cache )

## 113.43.00

- Use the ~log argument passed to `Client_simple.start` rather than
  `Log.Global.log` in one spot.

## 113.33.00

- Allow an smtp\_client to connect to a local unix socket

- Better logging

## 113.24.00

- Switched to PPX.

- Follow Core & Async evolution.

## 113.00.00

- Improve the async\_smtp client interface so that it is suitable as a
  replacement for Core\_extended.Std.Sendmail.

## 112.17.00

Moved from janestreet-alpha

