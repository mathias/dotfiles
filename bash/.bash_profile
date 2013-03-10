. "$HOME/.bashrc"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# define the pair function
source "$HOME/.pair"
# quietly set the previous pairing state
pair -q
