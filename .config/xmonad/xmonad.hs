-- Base
import           System.Exit                    (exitSuccess)
import           XMonad
import           XMonad.Hooks.EwmhDesktops
import qualified XMonad.StackSet                as W

-- Contrib
import           XMonad.Actions.PhysicalScreens
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Layout.Groups.Helpers
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.SimpleFloat
import           XMonad.Layout.Spacing
import           XMonad.Util.EZConfig
import           XMonad.Util.Loggers
import           XMonad.Util.Run

-- import Data.Default

-- Spacing function

mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Layouts

tall     = named "tall"
          $ mySpacing 7
          $ Tall 1 (3/100) (1.07/2)

monocle  = named "monocle"
           -- $ smartBorders
           -- $ noBorders
           $ Full

floating = named "float"
          $ simpleFloat

fullscreen = named "fullscreen"
             $ noBorders
             $ Full

myLayoutHook = tall ||| monocle ||| floating ||| fullscreen

-- Events

-- myEventHook = FS.fullscreenEventHook

-- Manage

-- myManageHook = FS.fullscreenManageHook

-- xmobar
myPP :: PP
myPP = def
  { ppSep = red " "
  , ppOrder = myOrder
  }
  where
    myOrder [ws, l, _, wins] = [ws, l, wins]
    red = xmobarColor "#ff5555" ""

main :: IO ()
main = xmonad
       . ewmhFullscreen
       . ewmh
       . withEasySB (statusBarProp "xmobar" (pure myPP)) defToggleStrutsKey
       $ def {
        modMask = mod4Mask,
        borderWidth = 2,
        -- handleEventHook = myEventHook,
        -- manageHook = myManageHook,
        layoutHook = myLayoutHook
    }
    `additionalKeys`
    [ ((mod4Mask, xK_period), onPrevNeighbour def W.view)
    , ((mod4Mask, xK_comma), onNextNeighbour def W.view)
    , ((mod4Mask .|. shiftMask, xK_period), onPrevNeighbour def W.shift)
    , ((mod4Mask .|. shiftMask, xK_comma), onNextNeighbour def W.shift)
    , ((mod4Mask .|. mod1Mask, xK_q), io exitSuccess)
    ]
    `additionalKeysP`
    [ ("M-q", kill)
    , ("M-<Space>", swapMaster)
    , ("M-C-<Space>", withFocused $ windows . W.sink)
    , ("M-<Comma>", onPrevNeighbour def W.view)
    , ("M-t", sendMessage $ JumpToLayout "tall")
    , ("M-y", sendMessage $ JumpToLayout "monocle")
    , ("M-u", sendMessage $ JumpToLayout "float")
    , ("M-i", sendMessage $ JumpToLayout "fullscreen")
    ]
    `removeKeysP`
    [ ("M-S-c")
    , ("M-?")
    , ("M-n")
    , ("M-S-q")
    , ("M-w")
    , ("M-e")
    , ("M-r")
    , ("M-p")
    , ("M-S-p")
    , ("M-<Return>")
    , ("M-S-<Return>")
    -- , ("M-<Space>")
    , ("M-S-<Space>")
    , ("M-<Tab>")
    , ("M-S-<Tab>")
    -- , ("M-t")
    ]
