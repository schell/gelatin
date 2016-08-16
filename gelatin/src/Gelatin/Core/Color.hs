module Gelatin.Core.Color where

import Linear
import Data.Bits

type Color = V4 Float

maroon :: (Num a, Fractional a) => V4 a
maroon = V4 (128/255) (0/255) (0/255) 1

red :: (Num a, Fractional a) => V4 a
red = V4 (255/255) (0/255) (0/255) 1

orange :: (Num a, Fractional a) => V4 a
orange = V4 (255/255) (165/255) (0/255) 1

yellow,canary :: (Num a, Fractional a) => V4 a
yellow = V4 (255/255) (255/255) (0/255) 1
canary = yellow

olive :: (Num a, Fractional a) => V4 a
olive = V4 (128/255) (128/255) (0/255) 1

green :: (Num a, Fractional a) => V4 a
green = V4 0 (128/255) (0/255) 1

purple :: (Num a, Fractional a) => V4 a
purple = V4 (128/255) (0/255) (128/255) 1

fuchsia :: (Num a, Fractional a) => V4 a
fuchsia = V4 (255/255) (0/255) (255/255) 1

lime :: (Num a, Fractional a) => V4 a
lime = V4 0 (255/255) (0/255) 1

teal :: (Num a, Fractional a) => V4 a
teal = V4 0 (128/255) (128/255) 1

aqua :: (Num a, Fractional a) => V4 a
aqua = V4 0 (255/255) (255/255) 1

blue :: (Num a, Fractional a) => V4 a
blue = V4 0 (0/255) (255/255) 1

navy :: (Num a, Fractional a) => V4 a
navy = V4 0 (0/255) (128/255) 1

black :: (Num a, Fractional a) => V4 a
black = V4 0 (0/255) (0/255) 1

gray :: (Num a, Fractional a) => V4 a
gray = V4 (128/255) (128/255) (128/255) 1

grey :: (Num a, Fractional a) => V4 a
grey = gray

silver :: (Num a, Fractional a) => V4 a
silver = V4 (192/255) (192/255) (192/255) 1

white :: (Num a, Fractional a) => V4 a
white = V4 (255/255) (255/255) (255/255) 1

indianRed :: (Num a, Fractional a) => V4 a
indianRed = V4 (205/255) (92/255) (92/255) 1

lightCoral :: (Num a, Fractional a) => V4 a
lightCoral = V4 (240/255) (128/255) (128/255) 1

salmon :: (Num a, Fractional a) => V4 a
salmon = V4 (250/255) (128/255) (114/255) 1

darkSalmon :: (Num a, Fractional a) => V4 a
darkSalmon = V4 (233/255) (150/255) (122/255) 1

lightSalmon :: (Num a, Fractional a) => V4 a
lightSalmon = V4 (255/255) (160/255) (122/255) 1

crimson :: (Num a, Fractional a) => V4 a
crimson = V4 (220/255) (20/255) (60/255) 1

fireBrick :: (Num a, Fractional a) => V4 a
fireBrick = V4 (178/255) (34/255) (34/255) 1

darkRed :: (Num a, Fractional a) => V4 a
darkRed = V4 (139/255) (0/255) (0/255) 1

pink :: (Num a, Fractional a) => V4 a
pink = V4 (255/255) (192/255) (203/255) 1

lightPink :: (Num a, Fractional a) => V4 a
lightPink = V4 (255/255) (182/255) (193/255) 1

hotPink :: (Num a, Fractional a) => V4 a
hotPink = V4 (255/255) (105/255) (180/255) 1

deepPink :: (Num a, Fractional a) => V4 a
deepPink = V4 (255/255) (20/255) (147/255) 1

mediumVioletRed :: (Num a, Fractional a) => V4 a
mediumVioletRed = V4 (199/255) (21/255) (133/255) 1

paleVioletRed :: (Num a, Fractional a) => V4 a
paleVioletRed = V4 (219/255) (112/255) (147/255) 1

coral :: (Num a, Fractional a) => V4 a
coral = V4 (255/255) (127/255) (80/255) 1

tomato :: (Num a, Fractional a) => V4 a
tomato = V4 (255/255) (99/255) (71/255) 1

orangeRed :: (Num a, Fractional a) => V4 a
orangeRed = V4 (255/255) (69/255) (0/255) 1

darkOrange :: (Num a, Fractional a) => V4 a
darkOrange = V4 (255/255) (140/255) (0/255) 1

gold :: (Num a, Fractional a) => V4 a
gold = V4 (255/255) (215/255) (0/255) 1

lightYellow :: (Num a, Fractional a) => V4 a
lightYellow = V4 (255/255) (255/255) (224/255) 1

lemonChiffon :: (Num a, Fractional a) => V4 a
lemonChiffon = V4 (255/255) (250/255) (205/255) 1

lightGoldenrodYellow :: (Num a, Fractional a) => V4 a
lightGoldenrodYellow = V4 (250/255) (250/255) (210/255) 1

papayaWhip :: (Num a, Fractional a) => V4 a
papayaWhip = V4 (255/255) (239/255) (213/255) 1

moccasin :: (Num a, Fractional a) => V4 a
moccasin = V4 (255/255) (228/255) (181/255) 1

peachPuff :: (Num a, Fractional a) => V4 a
peachPuff = V4 (255/255) (218/255) (185/255) 1

paleGoldenrod :: (Num a, Fractional a) => V4 a
paleGoldenrod = V4 (238/255) (232/255) (170/255) 1

khaki :: (Num a, Fractional a) => V4 a
khaki = V4 (240/255) (230/255) (140/255) 1

darkKhaki :: (Num a, Fractional a) => V4 a
darkKhaki = V4 (189/255) (183/255) (107/255) 1

lavender :: (Num a, Fractional a) => V4 a
lavender = V4 (230/255) (230/255) (250/255) 1

thistle :: (Num a, Fractional a) => V4 a
thistle = V4 (216/255) (191/255) (216/255) 1

plum :: (Num a, Fractional a) => V4 a
plum = V4 (221/255) (160/255) (221/255) 1

violet :: (Num a, Fractional a) => V4 a
violet = V4 (238/255) (130/255) (238/255) 1

orchid :: (Num a, Fractional a) => V4 a
orchid = V4 (218/255) (112/255) (214/255) 1

magenta :: (Num a, Fractional a) => V4 a
magenta = V4 (255/255) (0/255) (255/255) 1

mediumOrchid :: (Num a, Fractional a) => V4 a
mediumOrchid = V4 (186/255) (85/255) (211/255) 1

mediumPurple :: (Num a, Fractional a) => V4 a
mediumPurple = V4 (147/255) (112/255) (219/255) 1

amethyst :: (Num a, Fractional a) => V4 a
amethyst = V4 (153/255) (102/255) (204/255) 1

blueViolet :: (Num a, Fractional a) => V4 a
blueViolet = V4 (138/255) (43/255) (226/255) 1

darkViolet :: (Num a, Fractional a) => V4 a
darkViolet = V4 (148/255) (0/255) (211/255) 1

darkOrchid :: (Num a, Fractional a) => V4 a
darkOrchid = V4 (153/255) (50/255) (204/255) 1

darkMagenta :: (Num a, Fractional a) => V4 a
darkMagenta = V4 (139/255) (0/255) (139/255) 1

indigo :: (Num a, Fractional a) => V4 a
indigo = V4 (75/255) (0/255) (130/255) 1

slateBlue :: (Num a, Fractional a) => V4 a
slateBlue = V4 (106/255) (90/255) (205/255) 1

darkSlateBlue :: (Num a, Fractional a) => V4 a
darkSlateBlue = V4 (72/255) (61/255) (139/255) 1

mediumSlateBlue :: (Num a, Fractional a) => V4 a
mediumSlateBlue = V4 (123/255) (104/255) (238/255) 1

greenYellow :: (Num a, Fractional a) => V4 a
greenYellow = V4 (173/255) (255/255) (47/255) 1

chartreuse :: (Num a, Fractional a) => V4 a
chartreuse = V4 (127/255) (255/255) (0/255) 1

lawnGreen :: (Num a, Fractional a) => V4 a
lawnGreen = V4 (124/255) (252/255) (0/255) 1

limeGreen :: (Num a, Fractional a) => V4 a
limeGreen = V4 (50/255) (205/255) (50/255) 1

paleGreen :: (Num a, Fractional a) => V4 a
paleGreen = V4 (152/255) (251/255) (152/255) 1

lightGreen :: (Num a, Fractional a) => V4 a
lightGreen = V4 (144/255) (238/255) (144/255) 1

mediumSpringGreen :: (Num a, Fractional a) => V4 a
mediumSpringGreen = V4 0 (250/255) (154/255) 1

springGreen :: (Num a, Fractional a) => V4 a
springGreen = V4 0 (255/255) (127/255) 1

mediumSeaGreen :: (Num a, Fractional a) => V4 a
mediumSeaGreen = V4 (60/255) (179/255) (113/255) 1

seaGreen :: (Num a, Fractional a) => V4 a
seaGreen = V4 (46/255) (139/255) (87/255) 1

forestGreen :: (Num a, Fractional a) => V4 a
forestGreen = V4 (34/255) (139/255) (34/255) 1

darkGreen :: (Num a, Fractional a) => V4 a
darkGreen = V4 0 (100/255) (0/255) 1

yellowGreen :: (Num a, Fractional a) => V4 a
yellowGreen = V4 (154/255) (205/255) (50/255) 1

oliveDrab :: (Num a, Fractional a) => V4 a
oliveDrab = V4 (107/255) (142/255) (35/255) 1

darkOliveGreen :: (Num a, Fractional a) => V4 a
darkOliveGreen = V4 (85/255) (107/255) (47/255) 1

mediumAquamarine :: (Num a, Fractional a) => V4 a
mediumAquamarine = V4 (102/255) (205/255) (170/255) 1

darkSeaGreen :: (Num a, Fractional a) => V4 a
darkSeaGreen = V4 (143/255) (188/255) (143/255) 1

lightSeaGreen :: (Num a, Fractional a) => V4 a
lightSeaGreen = V4 (32/255) (178/255) (170/255) 1

darkCyan :: (Num a, Fractional a) => V4 a
darkCyan = V4 0 (139/255) (139/255) 1

cyan :: (Num a, Fractional a) => V4 a
cyan = V4 0 (255/255) (255/255) 1

lightCyan :: (Num a, Fractional a) => V4 a
lightCyan = V4 (224/255) (255/255) (255/255) 1

paleTurquoise :: (Num a, Fractional a) => V4 a
paleTurquoise = V4 (175/255) (238/255) (238/255) 1

aquamarine :: (Num a, Fractional a) => V4 a
aquamarine = V4 (127/255) (255/255) (212/255) 1

turquoise :: (Num a, Fractional a) => V4 a
turquoise = V4 (64/255) (224/255) (208/255) 1

mediumTurquoise :: (Num a, Fractional a) => V4 a
mediumTurquoise = V4 (72/255) (209/255) (204/255) 1

darkTurquoise :: (Num a, Fractional a) => V4 a
darkTurquoise = V4 0 (206/255) (209/255) 1

cadetBlue :: (Num a, Fractional a) => V4 a
cadetBlue = V4 (95/255) (158/255) (160/255) 1

steelBlue :: (Num a, Fractional a) => V4 a
steelBlue = V4 (70/255) (130/255) (180/255) 1

lightSteelBlue :: (Num a, Fractional a) => V4 a
lightSteelBlue = V4 (176/255) (196/255) (222/255) 1

powderBlue :: (Num a, Fractional a) => V4 a
powderBlue = V4 (176/255) (224/255) (230/255) 1

lightBlue :: (Num a, Fractional a) => V4 a
lightBlue = V4 (173/255) (216/255) (230/255) 1

skyBlue :: (Num a, Fractional a) => V4 a
skyBlue = V4 (135/255) (206/255) (235/255) 1

lightSkyBlue :: (Num a, Fractional a) => V4 a
lightSkyBlue = V4 (135/255) (206/255) (250/255) 1

deepSkyBlue :: (Num a, Fractional a) => V4 a
deepSkyBlue = V4 0 (191/255) (255/255) 1

dodgerBlue :: (Num a, Fractional a) => V4 a
dodgerBlue = V4 (30/255) (144/255) (255/255) 1

cornflowerBlue :: (Num a, Fractional a) => V4 a
cornflowerBlue = V4 (100/255) (149/255) (237/255) 1

royalBlue :: (Num a, Fractional a) => V4 a
royalBlue = V4 (65/255) (105/255) (225/255) 1

mediumBlue :: (Num a, Fractional a) => V4 a
mediumBlue = V4 0 (0/255) (205/255) 1

darkBlue :: (Num a, Fractional a) => V4 a
darkBlue = V4 0 (0/255) (139/255) 1

midnightBlue :: (Num a, Fractional a) => V4 a
midnightBlue = V4 (25/255) (25/255) (112/255) 1

cornsilk :: (Num a, Fractional a) => V4 a
cornsilk = V4 (255/255) (248/255) (220/255) 1

blanchedAlmond :: (Num a, Fractional a) => V4 a
blanchedAlmond = V4 (255/255) (235/255) (205/255) 1

bisque :: (Num a, Fractional a) => V4 a
bisque = V4 (255/255) (228/255) (196/255) 1

navajoWhite :: (Num a, Fractional a) => V4 a
navajoWhite = V4 (255/255) (222/255) (173/255) 1

wheat :: (Num a, Fractional a) => V4 a
wheat = V4 (245/255) (222/255) (179/255) 1

burlyWood :: (Num a, Fractional a) => V4 a
burlyWood = V4 (222/255) (184/255) (135/255) 1

tan :: (Num a, Fractional a) => V4 a
tan = V4 (210/255) (180/255) (140/255) 1

rosyBrown :: (Num a, Fractional a) => V4 a
rosyBrown = V4 (188/255) (143/255) (143/255) 1

sandyBrown :: (Num a, Fractional a) => V4 a
sandyBrown = V4 (244/255) (164/255) (96/255) 1

goldenrod :: (Num a, Fractional a) => V4 a
goldenrod = V4 (218/255) (165/255) (32/255) 1

darkGoldenrod :: (Num a, Fractional a) => V4 a
darkGoldenrod = V4 (184/255) (134/255) (11/255) 1

peru :: (Num a, Fractional a) => V4 a
peru = V4 (205/255) (133/255) (63/255) 1

chocolate :: (Num a, Fractional a) => V4 a
chocolate = V4 (210/255) (105/255) (30/255) 1

saddleBrown :: (Num a, Fractional a) => V4 a
saddleBrown = V4 (139/255) (69/255) (19/255) 1

sienna :: (Num a, Fractional a) => V4 a
sienna = V4 (160/255) (82/255) (45/255) 1

brown :: (Num a, Fractional a) => V4 a
brown = V4 (165/255) (42/255) (42/255) 1

snow :: (Num a, Fractional a) => V4 a
snow = V4 (255/255) (250/255) (250/255) 1

honeydew :: (Num a, Fractional a) => V4 a
honeydew = V4 (240/255) (255/255) (240/255) 1

mintCream :: (Num a, Fractional a) => V4 a
mintCream = V4 (245/255) (255/255) (250/255) 1

azure :: (Num a, Fractional a) => V4 a
azure = V4 (240/255) (255/255) (255/255) 1

aliceBlue :: (Num a, Fractional a) => V4 a
aliceBlue = V4 (240/255) (248/255) (255/255) 1

ghostWhite :: (Num a, Fractional a) => V4 a
ghostWhite = V4 (248/255) (248/255) (255/255) 1

whiteSmoke :: (Num a, Fractional a) => V4 a
whiteSmoke = V4 (245/255) (245/255) (245/255) 1

seashell :: (Num a, Fractional a) => V4 a
seashell = V4 (255/255) (245/255) (238/255) 1

beige :: (Num a, Fractional a) => V4 a
beige = V4 (245/255) (245/255) (220/255) 1

oldLace :: (Num a, Fractional a) => V4 a
oldLace = V4 (253/255) (245/255) (230/255) 1

floralWhite :: (Num a, Fractional a) => V4 a
floralWhite = V4 (255/255) (250/255) (240/255) 1

ivory :: (Num a, Fractional a) => V4 a
ivory = V4 (255/255) (255/255) (240/255) 1

antiqueWhite :: (Num a, Fractional a) => V4 a
antiqueWhite = V4 (250/255) (235/255) (215/255) 1

linen :: (Num a, Fractional a) => V4 a
linen = V4 (250/255) (240/255) (230/255) 1

lavenderBlush :: (Num a, Fractional a) => V4 a
lavenderBlush = V4 (255/255) (240/255) (245/255) 1

mistyRose :: (Num a, Fractional a) => V4 a
mistyRose = V4 (255/255) (228/255) (225/255) 1

gainsboro :: (Num a, Fractional a) => V4 a
gainsboro = V4 (220/255) (220/255) (220/255) 1

lightGrey :: (Num a, Fractional a) => V4 a
lightGrey = V4 (211/255) (211/255) (211/255) 1

darkGray :: (Num a, Fractional a) => V4 a
darkGray = V4 (169/255) (169/255) (169/255) 1

dimGray :: (Num a, Fractional a) => V4 a
dimGray = V4 (105/255) (105/255) (105/255) 1

lightSlateGray :: (Num a, Fractional a) => V4 a
lightSlateGray = V4 (119/255) (136/255) (153/255) 1

slateGray :: (Num a, Fractional a) => V4 a
slateGray = V4 (112/255) (128/255) (144/255) 1

darkSlateGray :: (Num a, Fractional a) => V4 a
darkSlateGray = V4 (47/255) (79/255) (79/255) 1

transparent :: (Num a, Fractional a) => V4 a
transparent = V4 0 0 0 0

withAlpha :: (Num a, Fractional a) => V4 a -> a -> V4 a
withAlpha (V4 r g b _) = V4 r g b

fromHex :: (Num b, Fractional b) => Int -> V4 b
fromHex n = ((/255) . fromIntegral) <$> V4 r g b a
    where r = n `shiftR` 24
          g = n `shiftR` 16 .&. 0xFF
          b = n `shiftR` 8 .&. 0xFF
          a = n .&. 0xFF
