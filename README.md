# dank-mode

Emacs major mode for browsing ~~dank memes~~ Reddit.

# Features and screenshots

- Load list of posts for your frontpage or a specific subreddit.
- Read comments for a post (nested and filled).
- Open posts and links in a browser or with EWW.
- Customizable faces.

![dank-mode-posts mode](screenshots/dank-mode-posts.png)
![dank-mode-comments mode](screenshots/dank-mode-comments.png)
![links](screenshots/link.png)

# Install

## MELPA

Coming soon.

## straight.el

You can also install the package with straight.el, just watch out for
[the issue with conflicting repository
names](https://github.com/radian-software/straight.el/issues/500).

Example with straight.el & use-package:

```elisp
(use-package web-server
  :defer t
  :straight (web-server :host github :repo "eschulte/emacs-web-server" :local-repo "web-server"))

(use-package dank-mode
  :straight (:host github :repo "john2x/dank-mode" :files ("lisp/*.el")))
```

## Manual installation

Download a release from https://github.com/john2x/dank-mode/releases
or clone this repo and copy the contents of the `lisp/` folder in your
Emacs load path.

## Minimum Emacs version

This package requires at least Emacs version 27.1.

## 3rd-party packages

This package requires the following packages (available on GNU ELPA):

- oauth2 (available on GNU ELPA)
- web-server (available on GNU ELPA)
- markdown-mode (available on non-GNU ELPA)

# Authentication

All requests to the Reddit API must be authenticated.

When you start `dank-mode` for the first time, you will be
redirected to the Reddit OAuth authorization page in your browser.

When you grant the permissions, you will be redirected to page served
on localhost. This server will parse the token in the page and store
it with `plstore`. At this point you will be prompted to enter a
passphrase to encrypt the token on disk.

You will occasionally be asked to re-enter the passphrase when the
token is refreshed (every 1 hour) or when you restart `dank-mode`.
(If you don't want to be prompted for a passphrase every time the
token needs to be refreshed, this can be disabled by setting
`dank-mode-oauth-encrypt-plstore` to `nil`)

Also watch out if you're using [EXWM](https://github.com/ch11ng/exwm),
because the OAuth execution won't let you press "Accept" in the
browser. You'd have to run `dank-mode` in a nested Emacs session for
the first time.

**Note about previous authentication method:** if you previously
authenticated with your own Reddit client id and username/password
(e.g. version 0.1.5 and below), you will need to re-authenticate.
This can be done by restarting `dank-mode` or with `M-x
dank-mode-oauth-start`.

## Storing your access/refresh tokens unecrypted

The `oauth2` package uses `plstore` to store the OAuth tokens on disk.
By default, it treats the access and refresh tokens as secrets
(appropriately so). This causes `plstore-put` to prompt for a
passphrase when writing them to disk.

This could get annoying when you have to provide a passphrase every
time the access token needs a refresh (every hour or so).

As a workaround, you can set `dank-mode-oauth-encrypt-plstore` to override
this behavior at your risk.

## Using your own Reddit API keys

By default, dank-mode will use a Reddit OAuth "installed" app created
by me. If you don't want to authorize this app, you can create your
own "installed" app via https://old.reddit.com/prefs/apps. Just make
sure to use `http://localhost:36227/dank-mode/oauth/redirect` as the
redirect url.

Replace `dank-mode-oauth-client-id` with your installed app client id.

# Usage instructions

## Quick tutorial

### Frontpage and subreddits

Open your frontpage with `M-x dank-mode`. If this is your first time
running `dank-mode`, you will be prompted to grant permissions (see
**Authentication** above). You will then be taken to a new buffer with
the frontpage posts sorted by `hot` (the default).

This buffer will have the `dank-mode-posts-mode` major mode enabled.
The "active" post under `point` will be highlighted.
Navigate between posts with `n` (down) and `p` (up).

To open the post link in your browser, press `C-x l b`. Alternatively,
to open the post link with `EWW`, press `C-x l l`.

To navigate to a specific subreddit, press `C-x C-f`. You will be
prompted to enter the subreddit name. A list of your subscribed
subreddits will be available for autocompletion, but specifying a
subreddit not in this list will also work.

Alternatively, to navigate to the subreddit under `point`, press `C-x
C-/`.

To load the next page, press `C-x C-v`. To refresh, press `C-x C-r`
(this will reset the page count).

To change the posts sorting, press `C-x C-s`.

### Comments

To read the comments of the active post, press `C-x C-o`. This will
create a new buffer in `dank-mode-comments` mode for the post's comments.
Navigating between comments is done similarly with `n` and `p`.

To jump between sibling comments, press `M-n` or `M-p`.  To jump to
the parent comment, press `P` (`Shift-p`).

To load more comments, navigate to that "load more" comment and press
`C-x C-o`. If it's a small enough comment tree the comments will be
inserted into the current buffer. If it's not, the comment tree will
be opened in a new buffer.

To fold/unfold a comment tree, press `TAB`.

To quit the comment buffer and go back to the frontpage or subreddit
buffer, press `C-x q`.

### Other commands

To view the full list of commands, refer to the mode help page `C-x h m`.

# Roadmap

- [ ] Better auth (e.g. don't require providing password in plaintext)
- [ ] Multi-reddits
- [ ] Upvote/downvote posts and comments
- [ ] Reply in comments
- [ ] Submitting posts
- [ ] Search

See `TODO.org` and `CHANGELOG.org` for more details.

## Maybe:

- Saving posts and comments
- Viewing the user's saved/upvoted/replied posts and comments

## Out of scope:

These are features I don't use and don't intend to implement anytime soon:

- moderating
- live threads
- polling
- private messages
