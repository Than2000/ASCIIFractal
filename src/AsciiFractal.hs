module AsciiFractal (afMain) where

import Data.Complex
import Data.Maybe
import Data.Either
--import System.Random
import Text.Read

--TODO Fix Command prompts
--TODO Add relative-to-screen-size translation
--TODO Implement aliases or "did you mean" suggestions, ex. q => quit
--TODO Display help messages within rejectArgs
--TODO Finish sierpinski
--TODO Welcome message
--TODO? Auto-detect screen size?
--TODO? Use different ascii chars to represent smaller regions (ex. dot if only one neighbor succeeded)

afMain = do
  putStrLn "Compiled."
  putStrLn "\n\nWelcome to the ASCII-Based Fractal Generator!\nType \"help\" to see available commands, or type \"draw\" to begin."
  run defaultGD ["0", "0"]


--Datatypes/typeclasses/etc.


data Vector2 = V2 Double Double deriving (Show, Read, Eq)
data Transformation = Transformation Matrix Vector2 deriving (Show, Read, Eq)
data GlobalData = GD { getWindowSize :: (Int, Int),
                       getTickSizes :: (Double, Double),
                       getPlotLoc :: (Vector2, Vector2),
                       getIters :: Int,
                       getAxesShown :: Bool,
                       getVectToChar :: VectToChar,
                       getAutodraw :: Bool}

type Matrix = (Double, Double, Double, Double)
type Args = [String]
type StrCommand = String
type Command = GlobalData -> Args -> IO ()
type PlotCommand = GlobalData -> Args -> Either (IO ()) VectToChar
type VectToChar = GlobalData -> Vector2 -> Char


--Constants


defaultGD :: GlobalData
defaultGD = GD (100,30) (1.0, 1.0) (V2 (-1.5) (-1), V2 1.5 1) 50 True vtcMBSet False

charWidth :: Double
charWidth = 0.3

cursorChar    = 'o'
originChar    = '+'
hAxisMinChar  = '<'
hAxisMaxChar  = '>'
hAxisChar     = '-'
hAxisTickChar = '|'
vAxisMinChar  = 'V'
vAxisMaxChar  = '^'
vAxisChar     = '|'
vAxisTickChar = '-'
maxShadeChar  = '#'
minShadeChar  = ' '
vBounderyChar = '|'
hBounderyChar = '~'

shadeChars :: [Char]
shadeChars = " `.*+&$@#"

commandList :: [(StrCommand, (Command, String))]
commandList = [("draw", (draw, "draw -- Redraws the plot to the screen.")),
               ("plot", (plot, "plot (args) -- Sets the function defining which points to plot.\nPossible arguments:\n" ++ show (map fst plotList))),

               ("zoom", (zoom, "zoom [Double c] -- Zooms in by the specified factor."
                             ++"Requires c > 0. Omitting arguments sets zoom level.")),
               ("shift", (shift, "shift (Double dx) (Double dy) -- Translates the plot by the specified amount.")),
               ("center", (center, "center (Double x) (Double y) -- Centers the plot on the point (x,y). Defaults to the origin.")),
               ("resize", (resize, "resize (Int width) (Int height) -- Resizes the window. Omitting arguments defaults to 100x30."
                                 ++"\nRequires width, height > 0. Tag -square ignores second argument and calculates height based on given width.")),
               ("toggleAxes", (toggleAxes, "toggleAxes -- Toggles the visibility of the axes.")),
               ("cutoff", (cutoff, "cutoff (Int n) -- Sets the cutoff point for iterations. Generally, a higher cutoff point means greater precision.\n"
                                 ++"Requires n > 0. Omitting arguments defaults to 50.")),

               ("reset", (resetValues, "reset -- Restores all data (center, scale factor, etc.) to their default values.")),
               ("info", (info, "info -- Prints information about the current plot.")),
               ("quit", (quit, "quit -- Ends execution. (Current data will be lost.)")),
               ("help", (help, "help [command] -- Prints a description of the specified command and its arguments. Omitting arguments lists available commands.")),
               ("toggleAutodraw", (toggleAutodraw, "toggleAutodraw -- Toggles redrawing after executing a command."))
               ]

plotList :: [(StrCommand, (PlotCommand, String))]
plotList = [("mandelbrot", (plotMBSet, "plot mandelbrot -- Plots the Mandelbrot set over the complex plane.\nTag -shade adds shading based on time to diverge.")),
            ("sierpinski", (plotSierpinski, "plot sierpinski -- Work in progress. Plots the Sierpinski triangle within the unit square, calculated as an iterated function system.\n")),
            ("juliaset", (plotJuliaSet, "plot juliaset (Complex c) -- Given complex c (using a:+b), plots the filled julia set of z^2+c over the complex plane.\n"
                                        ++"Omitting arguments defaults to 0:+0. Tag -shade adds shading based on time to diverge.\n"))
            ]
--NYI: Command "help plot [func]"


--IO commands


getCommand :: Command
getCommand gd _ = do
  line <- getLine
  if line == "" then do getCommand gd []
  else do
    let (strCmd:args) = words line
    let mCommand = lookup strCmd commandList
    case mCommand of Nothing -> rejectCommand gd [strCmd]
                     Just (command, description) -> command gd args
    return ()

rejectCommand :: Command
rejectCommand gd [strCmd] = do
  putStrLn $ "No such command: " ++ strCmd
  putStrLn "Use the command \"help\" to see a list of available commands."
  getCommand gd []
rejectCommand gd args = do
  putStrLn "No such command.\nUse the command \"help\" to see a list of available commands."
  getCommand gd []

rejectArgs :: Command
rejectArgs gd args = do
  putStrLn $ "Invalid arguments: " ++ unwords args ++ "\nUse the command \"help [command]\" for command syntax."
  getCommand gd []

run :: Command
run gd@ GD{getAutodraw=autodraw} _ = do
  if autodraw then do
    draw gd []
  else do
    getCommand gd []



--Input Commands


draw :: Command
draw gd _ = do
  putStr . unlines . asciiArray $ gd
  getCommand gd []

plot :: Command
plot gd args@(strPlot:args') = do
  let mPlotPair = lookup strPlot plotList
  if isNothing mPlotPair then do
    rejectArgs gd args
  else do
    let Just (plotCmd, description) = mPlotPair
    if isLeft $ plotCmd gd args' then do
      let Left failureAction = plotCmd gd args
      failureAction
    else do
      let Right command = plotCmd gd args'
      run (setVectToChar gd command) [""]
plot gd args = help gd $ "plot":args

zoom :: Command
zoom gd [] = run (setPlotLoc gd $ getPlotLoc defaultGD) []
zoom gd args
  | head args == "in" = zoom gd $ "2" : tail args
  | head args == "out" = zoom gd $ "0.5" : tail args
  | otherwise = do
    let mDouble = readMaybe (head args) :: Maybe Double
    case mDouble of Nothing -> rejectArgs gd args
                    Just x  ->  if x <= 0 then rejectArgs gd args
                                else run (zoomGD gd x) $ tail args
    return ()

shift :: Command
shift gd@ GD{getPlotLoc=(v1, v2)} args@(dxStr:dyStr:args')
  | isNothing (readMaybe dxStr :: Maybe Double) = rejectArgs gd args
  | isNothing (readMaybe dyStr :: Maybe Double) = rejectArgs gd args
  | otherwise = run (setPlotLoc gd (v1 `vPlus` d, v2 `vPlus` d)) args'
      where d = V2 (read dxStr :: Double) (read dyStr :: Double)
shift gd args = rejectArgs gd args

center :: Command
center gd [] = center gd ["0","0"]
center gd@ GD{getPlotLoc=(v1, v2)} args@(xStr:yStr:args')
  | isNothing (readMaybe xStr :: Maybe Double) = rejectArgs gd args
  | isNothing (readMaybe yStr :: Maybe Double) = rejectArgs gd args
  | otherwise = shift gd $ [show dx, show dy] ++ args'
      where (V2 dx dy) = newCenter `vMinus` currentCenter
            currentCenter = midPt v1 v2
            newCenter = V2 (read xStr :: Double) (read yStr :: Double)
center gd args = rejectArgs gd args

resize :: Command
resize gd [] = run (setWindowSize gd $ getWindowSize defaultGD) []
resize gd args@(wStr:hStr:args')
  | isNothing (readMaybe wStr :: Maybe Int) = rejectArgs gd args
  | width <= 0 = rejectArgs gd args
  | "square" `elem` args = resize gd sqrArgs
  | isNothing (readMaybe hStr :: Maybe Int) = rejectArgs gd args
  | height <= 0 = rejectArgs gd args
  | otherwise = run (setWindowSize gd (width, height)) args'
    where width = read wStr :: Int
          height = read hStr :: Int
          sqrHeight = round . (*charWidth) . fromIntegral $ width
          sqrArgs = wStr : show sqrHeight : filter (/= "square") args'
resize gd args = rejectArgs gd args

cutoff :: Command
cutoff gd [] = run (setIters gd $ getIters defaultGD) []
cutoff gd args@(strIters:args')
  | isNothing (readMaybe strIters :: Maybe Int) = rejectArgs gd args
  | iters <= 0 = rejectArgs gd args
  | otherwise = run (setIters gd iters) args'
    where iters = read strIters :: Int

toggleAxes :: Command
toggleAxes gd@ GD{getAxesShown=axesShown} = run (setAxesShown gd (not axesShown))

toggleAutodraw :: Command
toggleAutodraw gd@ GD{getAutodraw=autodraw} = run (setAutodraw gd (not autodraw))

resetValues :: Command --Shell command is "reset" - function name is resetValues instead because reset is already a declared funtion from Read.
resetValues _ = run defaultGD

info :: Command  --NYI: info for "plot" command
info gd args = do
  putStrLn "--Current plot information--"
  putStrLn $ "Window size: "++(show . fst . getWindowSize $ gd)++"x"++(show . snd . getWindowSize $ gd)
  let (xMin, yMin) = (getX . fst . getPlotLoc $ gd, getY . fst . getPlotLoc $ gd)
  let (xMax, yMax) = (getX . snd . getPlotLoc $ gd, getY . snd . getPlotLoc $ gd)
  putStrLn $ "Plot location (bounds): " ++ show xMin ++ " < x < " ++ show xMax ++ ",  " ++ show yMin ++ " < y < " ++ show yMax
  putStrLn $ "Cutoff point (number of iterations): " ++ show (getIters gd)
  putStrLn $ "Axes shown: " ++ show (getAxesShown gd)
  getCommand gd []

quit :: Command
quit gd args = do
  putStrLn "Quitting.\n\n"
  return ()

help :: Command
help gd ("plot":strPlot:args) = do
  let output = case lookup strPlot plotList of Just (f, desc) -> desc
                                               Nothing -> "No such plot: " ++ strPlot
  putStrLn output
  getCommand gd []

help gd [] = do
  putStrLn "Available commands: "
  print $ map fst commandList
  getCommand gd []

help gd args = do
  let strCmd = head args    
  let output = case lookup strCmd commandList of Just (f, desc) -> desc
                                                 Nothing -> "No such command: " ++ strCmd
  putStrLn output
  getCommand gd []


--GlobalData functions


setWindowSize :: GlobalData -> (Int, Int) -> GlobalData
setWindowSize (GD _ tickSizes plotLoc iters axesShown vectToChar autodraw) windowSize = GD windowSize tickSizes plotLoc iters axesShown vectToChar autodraw

setTickSizes :: GlobalData -> (Double, Double) -> GlobalData
setTickSizes (GD windowSize _ plotLoc iters axesShown vectToChar autodraw) tickSizes = GD windowSize tickSizes plotLoc iters axesShown vectToChar autodraw

setPlotLoc :: GlobalData -> (Vector2, Vector2) -> GlobalData
setPlotLoc (GD windowSize tickSizes _ iters axesShown vectToChar autodraw) plotLoc = GD windowSize tickSizes plotLoc iters axesShown vectToChar autodraw

setIters :: GlobalData -> Int -> GlobalData
setIters (GD windowSize tickSizes plotLoc _ axesShown vectToChar autodraw) iters = GD windowSize tickSizes plotLoc iters axesShown vectToChar autodraw

setAxesShown :: GlobalData -> Bool -> GlobalData
setAxesShown (GD windowSize tickSizes plotLoc iters _ vectToChar autodraw) axesShown = GD windowSize tickSizes plotLoc iters axesShown vectToChar autodraw

setVectToChar :: GlobalData -> VectToChar -> GlobalData
setVectToChar (GD windowSize tickSizes plotLoc iters axesShown _ autodraw) vectToChar = GD windowSize tickSizes plotLoc iters axesShown vectToChar autodraw

setAutodraw :: GlobalData -> Bool -> GlobalData
setAutodraw (GD windowSize tickSizes plotLoc iters axesShown vectToChar _) = GD windowSize tickSizes plotLoc iters axesShown vectToChar

pxlW :: GlobalData -> Double
pxlW gd = (getX vMax - getX vMin) / fromIntegral screenWidth
  where (screenWidth, _) = getWindowSize gd
        (vMin, vMax) = getPlotLoc gd

pxlH :: GlobalData -> Double
pxlH gd = (getY vMax - getY vMin) / fromIntegral screenHeight
  where (_, screenHeight) = getWindowSize gd
        (vMin, vMax) = getPlotLoc gd

zoomGD :: GlobalData -> Double -> GlobalData
zoomGD gd@GD{getPlotLoc=(v1, v2)} c = setPlotLoc gd (v1', v2')
  where v1' = vPlus (midPt v1 v2) . vScale (1/c) $ v1 `vMinus` midPt v1 v2
        v2' = vPlus (midPt v1 v2) . vScale (1/c) $ v2 `vMinus` midPt v1 v2


--Vector2 functions


getX :: Vector2 -> Double
getX (V2 x _) = x

getY :: Vector2 -> Double
getY (V2 _ y) = y

mag :: Vector2 -> Double
mag (V2 x y) = sqrt (x^2 + y^2)

vPlus :: Vector2 -> Vector2 -> Vector2
vPlus (V2 a b) (V2 x y) = V2 (a+x)  (b+y)

vMinus :: Vector2 -> Vector2 -> Vector2
vMinus (V2 a b) (V2 x y) = V2 (a-x)  (b-y)

vScale ::  Double -> Vector2 -> Vector2
vScale c (V2 x y) = V2 (x*c)  (y*c)

midPt :: Vector2 -> Vector2 -> Vector2
midPt v1 v2 = vScale 0.5 (v1 `vPlus` v2)

applyTransformation :: Transformation -> Vector2 -> Vector2
applyTransformation (Transformation (a11, a21, a12, a22) (V2 bx by)) v@(V2 x y)
  = V2 (a11*x + a12*y + bx) (a21*x + a22*y + by)

-- Printing


hRange :: GlobalData -> [Double]
hRange gd = [ xMin + i*dx | i <- [0..width] ]
  where dx = (xMax - xMin) / width
        width = fromIntegral . fst . getWindowSize $ gd
        xMin = getX . fst . getPlotLoc $ gd
        xMax = getX . snd . getPlotLoc $ gd

vectRow :: GlobalData -> Double -> [Vector2]
vectRow gd y = map (`V2` y) $ hRange gd

vRange :: GlobalData -> [Double]
vRange gd = reverse [ yMin + i*dy | i <- [0..height] ]
  where dy = (yMax - yMin) / height
        height = fromIntegral . snd . getWindowSize $ gd
        yMin = getY . fst . getPlotLoc $ gd
        yMax = getY . snd . getPlotLoc $ gd

vectArray :: GlobalData -> [[Vector2]]
vectArray gd = map (vectRow gd) $ vRange gd

asciiArray :: GlobalData -> [[Char]]
asciiArray gd = addBoundery . map (map (vectToAscii gd)) $ vectArray gd

addBoundery :: [[Char]] -> [[Char]]
addBoundery array = [hBoundery] ++ addVBoundery array ++ [hBoundery]
  where hBoundery = minShadeChar : replicate (length . head $ array) hBounderyChar ++ [minShadeChar]
        addVBoundery array = map (\row -> vBounderyChar : row ++ [vBounderyChar] ) array


vectToAscii :: GlobalData -> Vector2 -> Char
vectToAscii gd v@(V2 x y)
  | onCursor                               = cursorChar
  | onXAxis && onYAxis                     = originChar

  -- x-axis
  | onXAxis && abs (x-xMin) <= pxlW gd / 2 = hAxisMinChar
  | onXAxis && abs (x-xMax) <= pxlW gd / 2 = hAxisMaxChar
  | onXTick                                = hAxisTickChar
  | onXAxis                                = hAxisChar

  -- y-axis
  | onYAxis && abs (y-yMin) <= pxlH gd / 2 = vAxisMinChar
  | onYAxis && abs (y-yMax) <= pxlH gd / 2 = vAxisMaxChar
  | onYTick                                = vAxisTickChar
  | onYAxis                                = vAxisChar
  
  | otherwise                              = getVectToChar gd gd v

  where (scrnW,scrnH) = getWindowSize gd
        (vMin, vMax) = getPlotLoc gd
        onXAxis = getAxesShown gd && abs y < pxlH gd / 2
        onYAxis = getAxesShown gd && abs x < pxlW gd / 2
        onXTick = onXAxis && abs (x - rndX) < pxlW gd / 2      --WIP
        onYTick = onYAxis && abs (y - rndY) < pxlH gd / 2      --WIP
        [rndX, rndY] = map (fromIntegral . round) [x, y]       --WIP rewrite as nearest tick
        (xMin, yMin) = (getX vMin, getY vMin)
        (xMax, yMax) = (getX vMax, getY vMax)
        onCursor = mag (v `vMinus` midPt vMin vMax) <= pxlW gd / 2


--Mandelbrot set

plotMBSet :: PlotCommand
plotMBSet gd args
  | "-shade" `elem` args = Right vtcMBSetShaded
  | otherwise        = Right vtcMBSet

vtcMBSet :: VectToChar
vtcMBSet gd@GD{getIters=iters} (V2 x y)
  | isInSet   = maxShadeChar
  | otherwise = minShadeChar
   where isInSet = all (\z -> magnitude z < 2) . take iters $ mSequence c (0:+0)
         c = x:+y
         mSequence c z = z : mSequence c (mFunc c z)
         mFunc c z = z^2 + c

vtcMBSetShaded :: VectToChar
vtcMBSetShaded gd@GD{getIters=maxIters} (V2 x y)
  | itersToEscape >= maxIters             = maxShadeChar
  | itersToEscape >= (49*maxIters) `div` 8 = shadeChars !! 7
  | itersToEscape >= (36*maxIters) `div` 8 = shadeChars !! 6
  | itersToEscape >= (25*maxIters) `div` 8 = shadeChars !! 5
  | itersToEscape >= (16*maxIters) `div` 8 = shadeChars !! 4
  | itersToEscape >= (9*maxIters) `div` 8 = shadeChars !! 3
  | itersToEscape >= (4*maxIters) `div` 8 = shadeChars !! 2
  | itersToEscape >= (1*maxIters) `div` 8 = shadeChars !! 1
  | otherwise = minShadeChar
   where itersToEscape = length . takeWhile (\z -> magnitude z < 2) . take maxIters $ mSequence c (0:+0)
         c = x:+y
         mSequence c z = z : mSequence c (z^2 + c)


--Julia Sets

plotJuliaSet :: PlotCommand
plotJuliaSet gd args@(cStr:args')
  | isNothing mc = Left (rejectArgs gd args)
  | "-shade" `elem` args             = Right (vtcJuliaSetShaded c)
  | otherwise                    = Right (vtcJuliaSet c)
  where mc = readMaybe cStr :: Maybe (Complex Double)
        Just c = mc

plotJuliaSet gd args = plotJuliaSet gd ("0":"0":args)

vtcJuliaSet :: Complex Double -> VectToChar
vtcJuliaSet c gd@GD{getIters=iters} (V2 x y)
  | isInSet   = maxShadeChar
  | otherwise = minShadeChar
   where isInSet = all (\z -> magnitude z < 2) . take iters $ jSequence c (x:+y)
         jSequence c z = z : jSequence c (z^2 + c)

vtcJuliaSetShaded :: Complex Double -> VectToChar
vtcJuliaSetShaded c gd@GD{getIters=maxIters} (V2 x y)
  | itersToEscape >= maxIters             = maxShadeChar
  | itersToEscape >= (7*maxIters) `div` 8 = shadeChars !! 7
  | itersToEscape >= (6*maxIters) `div` 8 = shadeChars !! 6
  | itersToEscape >= (5*maxIters) `div` 8 = shadeChars !! 5
  | itersToEscape >= (4*maxIters) `div` 8 = shadeChars !! 4
  | itersToEscape >= (3*maxIters) `div` 8 = shadeChars !! 3
  | itersToEscape >= (2*maxIters) `div` 8 = shadeChars !! 2
  | itersToEscape >= (1*maxIters) `div` 8 = shadeChars !! 1
  | otherwise = minShadeChar
   where itersToEscape = length . takeWhile (\z -> magnitude z < 2) . take maxIters $ jSequence c (x:+y)
         jSequence c z = z : jSequence c (z^2 + c)

--Sierpinski triangle (On hold - random)

plotSierpinski :: PlotCommand
plotSierpinski gd args = Right vtcSierpinski

vtcSierpinski :: VectToChar
vtcSierpinski gd v@(V2 x y)
  | x<0 || x>1 || y<0 || y>1 = minShadeChar
  | any (withinRange gd v) $ truncatedList gd = maxShadeChar
  | otherwise = minShadeChar
truncatedList gd = take (getIters gd) sierpinskiList'  --where
withinRange gd (V2 x1 y1) (V2 x2 y2) = abs (x2-x1) <= pxlW gd && abs (y2-y1) <= pxlH gd  --where

sierpinskiList :: Vector2 -> [Vector2]
sierpinskiList v = v : (sierpinskiList . fst $ nextSierpinski (v, 2))

sierpinskiList' = sierpinskiList (V2 0.98356032478956 0.123845769574862)

nextSierpinski :: (Vector2, Int) -> (Vector2, Int)
nextSierpinski (v@(V2 x y), n)
  | rand n `mod` 3 == 0 = (applyTransformation trans0 v, n+1)
  | rand n `mod` 3 == 1 = (applyTransformation trans1 v, n+1)
  | otherwise = (applyTransformation trans2 v, n+1)
  where trans0 = Transformation (0.5, 0.0, 0.0, 0.5) (V2 0 0)
        trans1 = Transformation (0.5, 0.0, 0.0, 0.5) (V2 0.5 0)
        trans2 = Transformation (0.5, 0.0, 0.0, 0.5) (V2 0.25 $ sqrt 3 / 4)

--rand x = ( (round x*1000) * 6146 + 1223) `mod` 1229          ---WIP
rand n
  | n < length rands = rands !! n
  | otherwise = 0

rands = [0,2,2,1,0,1,0,1,0,0,1,1,0,2,0,0,2,2,0,2,2,1,2,0,0,1,2,0,2,1,0,0]
