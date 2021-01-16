# Emacs doom configuration

## Install emacs

This is instalation without bar and with webkit support

```bash
brew install emacs-plus@28 --with-xwidgets --with-modern-papirus-icon --with-no-titlebar
```

For correct working with markdown pandoc should be installed

Mac os example

```bash
brew install pandoc
```

## Install Email dependencies

_Term_

```bash
brew install cmake
```

_Email_

```bash
brew install mu
brew install offlineimap
```

more information see [here](https://kirang.in/post/emacs-as-email-client-with-offlineimap-and-mu4e-on-osx/)

## Work with databases

```bash
cpan RPC::EPC::Service DBI DBD::SQLite DBD::Pg DBD::mysql
```
