if [ -z "$TMUX" ]
  set ids (tmux list-sessions)
  if [ -z "$ids" ]
    tmux new-session
  else
    tmux attach
  end
end
