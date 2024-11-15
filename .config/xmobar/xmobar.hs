import           Xmobar

config :: Config
config =
  defaultConfig
  { font = "Terminess Nerd Font Mono 9",
    -- font = "DejaVu Sans Mono 9",
    allDesktops = True,
    alpha = 200,
    iconRoot = "/.config/xmobar/assets",
    commands =
      [
        Run XMonadLog,
        Run $ Memory ["--template", "<usedratio>%"] 10,
        Run $ Kbd [],
        Run $ Date "%a %_d %b %Y <fc=#ee9a00>%H:%M:%S</fc>" "date" 10
      ],
    template = "%XMonadLog% }{ %kbd% | %date% | <icon=memory.xpm/>%memory%",
    alignSep = "}{"
  }

main :: IO ()
main = xmobar config  -- or: configFromArgs config >>= xmobar
