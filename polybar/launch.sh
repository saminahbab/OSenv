# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch Polybar, using default config location ~/.config/polybar/config


MONITOR=DisplayPort-2 polybar -c config main


MONITOR=DisplayPort-1 polybar -c config main

echo "Polybar launched..."
