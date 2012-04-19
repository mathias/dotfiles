# ryalnd/dotfiles
A collection of scripts and commands that I use every day.

##Contents

###scripts/pair

Configures the local git author/email for multiple developers when pair programming

#### Usage:

```bash
$ pair ryalnd mathias   # Sets the author to 'Matt Gauger and Ryland Herrick'
$ pair -u               # Unsets the local author/email
```

You can also set more than two users:

```bash
$ pair bigtiger devn mathias   # Sets the author to 'Jim Remsik, Devin Walters and Matt Gauger'
```

And check your current configuration:

```bash
$ pair -u               # Lists the current author/email
```
