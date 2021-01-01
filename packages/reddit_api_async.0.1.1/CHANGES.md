# 0.1.1 (2020-12-30)

## Added

- Add lots of documentation.

## Changed

- Increase the minimum delay between HTTP requests from 10ms to 100ms.
- Return a special `Inbox_item.Comment.t` value instead of a `Thing.Comment.t`
  for inbox endpoints.

## Removed

- Remove unused optional `subreddit` parameter from `Api.info`.
- Remove `Bounded_set` from the public interface of `reddit_api_async`.
