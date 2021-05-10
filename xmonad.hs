-----------------------------------------------------------------------
-- IMPORTS
-----------------------------------------------------------------------

   -- Base
import XMonad
import System.Directory
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

   -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

   -- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

   -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

   -- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

   -- Layout Modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout (Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

   -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Unicode
import XMonad.Prompt.XMonad
import Control.Arrow (first)

   -- Utilities
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad


-----------------------------------------------------------------------
-- VARIABLES
-----------------------------------------------------------------------

myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

myEmojiFont :: String
myEmojiFont = "xft:JoyPixels:regular:size=9:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask

myAltMask :: KeyMask
myAltMask = mod1Mask

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

myEmacs :: String
myEmacs = "emacsclient -c -a 'emacs'"

myEditor :: String
myEditor = myTerminal ++ " -e vim "
--myEditor = "emacsclient -c -a 'emacs'"

myBorderWidth :: Dimension
myBorderWidth = 1

myNormalBorderColor :: String
myNormalBorderColor = "#dddddd"

myFocusedBorderColor :: String
myFocusedBorderColor = "#ffb400"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "picom &"
  spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x282c34  --height 22 &"
  spawnOnce "volumeicon &"
  spawnOnce "emacs --daemon &"
  spawnOnce "feh --randomize --bg-fill ~/wallpapers/*" -- random wallpaper each session
  --spawnOnce "nitrogen --restore &"                   -- nitrogen wallpaper
  setWMName "LG3D"
-----------------------------------------------------------------------
-- GRID MENU
-----------------------------------------------------------------------

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x28,0x2c,0x34) -- lowest inactive bg
                  (0x28,0x2c,0x34) -- highes inactive bg
                  (0xc7,0x92,0xea) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0x28,0x2c,0x34) -- active fg

mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
   { gs_cellheight   = 40
   , gs_cellwidth    = 200
   , gs_cellpadding  = 6
   , gs_originFractX = 0.5
   , gs_originFractY = 0.5
   , gs_font         = myFont
   } 

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
   where conf = def
                  { gs_cellheight   = 40
                  , gs_cellwidth    = 200
                  , gs_cellpadding  = 6
                  , gs_originFractX = 0.5
                  , gs_originFractY = 0.5
                  , gs_font         = myFont
                  }

myAppGrid = [ ("Audacity", "audacity")
                 , ("Emacs","emacsclient -c -a emacs")
                 , ("Firefox","firefox")
                 , ("OBS","obs")
                 , ("Gimp","gimp")
                 ]


-----------------------------------------------------------------------
-- SCRATCHPADS
-----------------------------------------------------------------------

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "mocp" spawnMocp findMocp manageMocp
                , NS "calculator" spawnCalc findCalc manageCalc
                ]
  where
    spawnTerm  = myTerminal ++ " -t scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect 1 t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnMocp  = myTerminal ++ " -t mocp -e mocp"
    findMocp   = title =? "mocp"
    manageMocp = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnCalc  = "qalculate-gtk"
    findCalc   = className =? "Qalculate-gtk"
    manageCalc = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.4
                 t = 0.75 -h
                 l = 0.70 -w

-----------------------------------------------------------------------
-- KEY BINDS
-----------------------------------------------------------------------

myKeys :: [(String, X ())] 
myKeys = 
    -- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile")  -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")    -- Restarts xmonad
        , ("M-S-q", io exitSuccess)              -- Quits xmonad

    -- Run Prompt
        , ("M-S-<Return>", spawn "dmenu_run -i -p \"Run: \"") -- Dmenu

    -- Other Prompts
        , ("M-p a", spawn "dmsounds")  -- pick color from our scheme
        , ("M-p c", spawn "dmcolors")  -- pick color from our scheme
        , ("M-p e", spawn "dmconf")   -- edit config files
        , ("M-p i", spawn "dmscrot")  -- screenshots (images)
        , ("M-p k", spawn "dmkill")   -- kill processes
        , ("M-p m", spawn "dman")     -- manpages
        , ("M-p o", spawn "dmqute")   -- qutebrowser bookmarks/history
        , ("M-p p", spawn "passmenu") -- passmenu
        , ("M-p q", spawn "dmlogout") -- logout menu
        , ("M-p r", spawn "dmred")    -- reddio (a reddit viewer)
        , ("M-p s", spawn "dmsearch") -- search various search engines

    -- Useful Programs
        , ("M-<Return>", spawn (myTerminal))
        , ("M-b", spawn (myBrowser ++ " www.youtube.com/c/DistroTube/"))
        , ("M-M1-h", spawn (myTerminal ++ " -e htop"))

    -- Kill Windows
        , ("M-S-c", kill1)     -- Kill the currently focused client
        , ("M-S-a", killAll)   -- Kill all windows on current workspace

    -- Workspaces
        , ("M-.", nextScreen)  -- Switch focus to next monitor
        , ("M-,", prevScreen)  -- Switch focus to prev monitor
        , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next ws
        , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to prev ws

    -- Floating windows
        , ("M-f", sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout
        , ("M-t", withFocused $ windows . W.sink)  -- Push floating window back to tile
        , ("M-S-t", sinkAll)                       -- Push ALL floating windows to tile

    -- Increase/decrease spacing (gaps)
        , ("C-M4-j", decWindowSpacing 4)         -- Decrease window spacing
        , ("C-M4-k", incWindowSpacing 4)         -- Increase window spacing
        , ("C-M4-h", decScreenSpacing 4)         -- Decrease screen spacing
        , ("C-M4-l", incScreenSpacing 4)         -- Increase screen spacing

    -- Grid Select (CTR-g followed by a key)
        , ("C-g a", spawnSelected' myAppGrid)                 -- grid select favorite apps
        , ("C-g t", goToSelected $ mygridConfig myColorizer)  -- goto selected window
        , ("C-g b", bringSelected $ mygridConfig myColorizer) -- bring selected window

    -- Windows navigation
        , ("M-m", windows W.focusMaster)  -- Move focus to the master window
        , ("M-j", windows W.focusDown)    -- Move focus to the next window
        , ("M-k", windows W.focusUp)      -- Move focus to the prev window
        , ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)   -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)     -- Swap focused window with prev window
        , ("M-<Backspace>", promote)      -- Moves focused window to master, others maintain order
        , ("M-S-<Tab>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
        , ("M-C-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack

    -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)           -- Switch to next layout
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full

    -- Window resizing
        , ("M-h", sendMessage Shrink)                   -- Shrink horiz window width
        , ("M-l", sendMessage Expand)                   -- Expand horiz window width
        , ("M-M1-j", sendMessage MirrorShrink)          -- Shrink vert window width
        , ("M-M1-k", sendMessage MirrorExpand)          -- Expand vert window width

    -- Sublayouts
    -- This is used to push windows to tabbed sublayouts, or pull them out of it.
        , ("M-C-h", sendMessage $ pullGroup L)
        , ("M-C-l", sendMessage $ pullGroup R)
        , ("M-C-k", sendMessage $ pullGroup U)
        , ("M-C-j", sendMessage $ pullGroup D)
        , ("M-C-m", withFocused (sendMessage . MergeAll))
        , ("M-C-/", withFocused (sendMessage . UnMergeAll))
        , ("M-C-.", onGroup W.focusUp')    -- Switch focus to next tab
        , ("M-C-,", onGroup W.focusDown')  -- Switch focus to prev tab

    -- Scratchpads
        , ("C-s t", namedScratchpadAction myScratchPads "terminal")
        , ("C-s m", namedScratchpadAction myScratchPads "mocp")
        , ("C-s c", namedScratchpadAction myScratchPads "calculator")

    -- Wallpaper with feh.
        , ("M-<F1>", spawn "sxiv -r -q -t -o ~/wallpapers/*")
        , ("M-<F2>", spawn "/bin/ls ~/wallpapers | shuf -n 1 | xargs xwallpaper --stretch")
        --, ("M-<F2>", spawn "feh --randomize --bg-fill ~/wallpapers/*")

    -- Emacs (CTRL-e followed by a key)
        -- , ("C-e e", spawn myEmacs)                 -- start emacs
        , ("C-e e", spawn (myEmacs ++ ("--eval '(dashboard-refresh-buffer)'")))   -- emacs dashboard
        , ("C-e b", spawn (myEmacs ++ ("--eval '(ibuffer)'")))   -- list buffers
        , ("C-e d", spawn (myEmacs ++ ("--eval '(dired nil)'"))) -- dired
        , ("C-e i", spawn (myEmacs ++ ("--eval '(erc)'")))       -- erc irc client
        , ("C-e m", spawn (myEmacs ++ ("--eval '(mu4e)'")))      -- mu4e email
        , ("C-e n", spawn (myEmacs ++ ("--eval '(elfeed)'")))    -- elfeed rss
        , ("C-e s", spawn (myEmacs ++ ("--eval '(eshell)'")))    -- eshell
        , ("C-e t", spawn (myEmacs ++ ("--eval '(mastodon)'")))  -- mastodon.el
        -- , ("C-e v", spawn (myEmacs ++ ("--eval '(vterm nil)'"))) -- vterm if on GNU Emacs
        , ("C-e v", spawn (myEmacs ++ ("--eval '(+vterm/here nil)'"))) -- vterm if on Doom Emacs
        -- , ("C-e w", spawn (myEmacs ++ ("--eval '(eww \"distrotube.com\")'"))) -- eww browser if on GNU Emacs
        , ("C-e w", spawn (myEmacs ++ ("--eval '(doom/window-maximize-buffer(eww \"distrotube.com\"))'"))) -- eww browser if on Doom Emacs
        -- emms is an emacs audio player. I set it to auto start playing in a specific directory.
        , ("C-e a", spawn (myEmacs ++ ("--eval '(emms)' --eval '(emms-play-directory-tree \"~/Music/Non-Classical/70s-80s/\")'")))
        ]
    -- Named Scratchpads Keys
      where nonNSP         = WSIs (return (\ws -> W.tag ws /= "NSP"))
            nonEmptyNonNSP = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))

-----------------------------------------------------------------------
-- MOUSE BINDS
-----------------------------------------------------------------------

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]


-----------------------------------------------------------------------
-- LAYOUTS
-----------------------------------------------------------------------

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Same as above but no borders are applied of fewer than two windows
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- limitWindows n sets maximum number of windows displayed for layout.
-- mySpacing n sets the gap size around the windows.
tall     = renamed [Replace "tall"]
           $ smartBorders
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 6 
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ smartBorders
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 20 Full
floats   = renamed [Replace "floats"]
           $ smartBorders
           $ limitWindows 20 simplestFloat
grid     = renamed [Replace "grid"]
           $ smartBorders
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
threeCol = renamed [Replace "threeCol"]
           $ smartBorders
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 7
           $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "tabs"]
           $ tabbed shrinkText myTabTheme
tallAccordion  = renamed [Replace "tallAccordion"]
           $ Accordion
wideAccordion  = renamed [Replace "wideAccordion"]
           $ Mirror Accordion

-- setting colors for tabs layout and tabs sublayout.
myTabTheme = def { fontName            = myFont
                 , activeColor         = "#46d9ff"
                 , inactiveColor       = "#313846"
                 , activeBorderColor   = "#46d9ff"
                 , inactiveBorderColor = "#282c34"
                 , activeTextColor     = "#282c34"
                 , inactiveTextColor   = "#d0d0d0"
                 }

myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =     withBorder myBorderWidth tall
                                 ||| noBorders monocle
                                 ||| grid
                                 ||| noBorders tabs
                                 ||| floats
                                 ||| threeCol
                                 ||| tallAccordion
                                 ||| wideAccordion


-----------------------------------------------------------------------
-- CONFIG VARIABLES
-----------------------------------------------------------------------

myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font    = "xft:Ubuntu:bold:size=60"
    , swn_fade    = 1.0
    , swn_bgcolor = "#1c1f24"
    , swn_color   = "#ffffff"
    }

myWorkspaces = [" dev ", " WWW ", " sys ", " doc ", " mpv ", " gfx ", " vbox ", " irc ", " zoom "]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

-- > xprop | grep WM_CLASS
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ className =? "confirm"        --> doFloat
     , className =? "file_progress"  --> doFloat
     , className =? "dialog"         --> doFloat
     , className =? "download"       --> doFloat
     , className =? "xmessage"       --> doFloat
     , className =? "error"          --> doFloat
     , className =? "Gimp"           --> doFloat
     , className =? "notification"   --> doFloat
     , className =? "splash"         --> doFloat
     , className =? "toolbar"        --> doFloat
     , className =? "Mplayer"        --> doFloat
     , title =? "Oracle VM VirtualBox Manager" --> doFloat
     , title =? "Mozilla Firefox"    --> doShift ( myWorkspaces !! 1 )
     , className =? "Gimp"           --> doShift ( myWorkspaces !! 5 )
     , className =? "mpv"            --> doShift ( myWorkspaces !! 4 )
     , className =? "VirtualBox Manager" --> doShift ( myWorkspaces !! 6 )
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat -- Firefox Dialogls
     , isFullscreen --> doFullFloat
     ] <+> namedScratchpadManageHook myScratchPads


main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar ~/.xmobarrc"
  xmonad $ ewmh def
      { manageHook         = myManageHook <+> manageDocks
      , handleEventHook    = docksEventHook
      , modMask            = myModMask
      , terminal           = myTerminal
      , startupHook        = myStartupHook
      , layoutHook         = showWName' myShowWNameTheme $ myLayoutHook
      , focusFollowsMouse  = myFocusFollowsMouse
      , clickJustFocuses   = myClickJustFocuses
      , borderWidth        = myBorderWidth
      , workspaces         = myWorkspaces
      , normalBorderColor  = myNormalBorderColor
      , focusedBorderColor = myFocusedBorderColor
      , mouseBindings      = myMouseBindings
      , logHook            = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ xmobarPP
            -- 'pp' is for xmobar
            { ppOutput = \x -> hPutStrLn xmproc x
            , ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]"
            , ppVisible = xmobarColor "#98be65" "" . clickable
            , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" "" . clickable
            , ppHiddenNoWindows = xmobarColor "#c792ea" "" . clickable
            , ppTitle = xmobarColor "#b3afc2" "" . shorten 60
            , ppSep = "<fc=#666666> <fn=1>|</fn> </fc>"
            , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
            , ppExtras = [windowCount]
            , ppOrder = \(ws:l:t:ex) -> [ws,l]++ex++[t]
            }

      } `additionalKeysP` myKeys
