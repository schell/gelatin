module Gelatin.Color where

import Graphics.Rendering.OpenGL
import Linear

maroon :: (Num a, Fractional a) => Color4 a
maroon = Color4 (128/255) (0/255) (0/255) 1

red :: (Num a, Fractional a) => Color4 a
red = Color4 (255/255) (0/255) (0/255) 1

orange :: (Num a, Fractional a) => Color4 a
orange = Color4 (255/255) (165/255) (0/255) 1

yellow :: (Num a, Fractional a) => Color4 a
yellow = Color4 (255/255) (255/255) (0/255) 1

olive :: (Num a, Fractional a) => Color4 a
olive = Color4 (128/255) (128/255) (0/255) 1

green :: (Num a, Fractional a) => Color4 a
green = Color4 0 (128/255) (0/255) 1

purple :: (Num a, Fractional a) => Color4 a
purple = Color4 (128/255) (0/255) (128/255) 1

fuchsia :: (Num a, Fractional a) => Color4 a
fuchsia = Color4 (255/255) (0/255) (255/255) 1

lime :: (Num a, Fractional a) => Color4 a
lime = Color4 0 (255/255) (0/255) 1

teal :: (Num a, Fractional a) => Color4 a
teal = Color4 0 (128/255) (128/255) 1

aqua :: (Num a, Fractional a) => Color4 a
aqua = Color4 0 (255/255) (255/255) 1

blue :: (Num a, Fractional a) => Color4 a
blue = Color4 0 (0/255) (255/255) 1

navy :: (Num a, Fractional a) => Color4 a
navy = Color4 0 (0/255) (128/255) 1

black :: (Num a, Fractional a) => Color4 a
black = Color4 0 (0/255) (0/255) 1

gray :: (Num a, Fractional a) => Color4 a
gray = Color4 (128/255) (128/255) (128/255) 1

silver :: (Num a, Fractional a) => Color4 a
silver = Color4 (192/255) (192/255) (192/255) 1

white :: (Num a, Fractional a) => Color4 a
white = Color4 (255/255) (255/255) (255/255) 1

indianRed :: (Num a, Fractional a) => Color4 a
indianRed = Color4 (205/255) (92/255) (92/255) 1

lightCoral :: (Num a, Fractional a) => Color4 a
lightCoral = Color4 (240/255) (128/255) (128/255) 1

salmon :: (Num a, Fractional a) => Color4 a
salmon = Color4 (250/255) (128/255) (114/255) 1

darkSalmon :: (Num a, Fractional a) => Color4 a
darkSalmon = Color4 (233/255) (150/255) (122/255) 1

lightSalmon :: (Num a, Fractional a) => Color4 a
lightSalmon = Color4 (255/255) (160/255) (122/255) 1

crimson :: (Num a, Fractional a) => Color4 a
crimson = Color4 (220/255) (20/255) (60/255) 1

fireBrick :: (Num a, Fractional a) => Color4 a
fireBrick = Color4 (178/255) (34/255) (34/255) 1

darkRed :: (Num a, Fractional a) => Color4 a
darkRed = Color4 (139/255) (0/255) (0/255) 1

pink :: (Num a, Fractional a) => Color4 a
pink = Color4 (255/255) (192/255) (203/255) 1

lightPink :: (Num a, Fractional a) => Color4 a
lightPink = Color4 (255/255) (182/255) (193/255) 1

hotPink :: (Num a, Fractional a) => Color4 a
hotPink = Color4 (255/255) (105/255) (180/255) 1

deepPink :: (Num a, Fractional a) => Color4 a
deepPink = Color4 (255/255) (20/255) (147/255) 1

mediumVioletRed :: (Num a, Fractional a) => Color4 a
mediumVioletRed = Color4 (199/255) (21/255) (133/255) 1

paleVioletRed :: (Num a, Fractional a) => Color4 a
paleVioletRed = Color4 (219/255) (112/255) (147/255) 1

coral :: (Num a, Fractional a) => Color4 a
coral = Color4 (255/255) (127/255) (80/255) 1

tomato :: (Num a, Fractional a) => Color4 a
tomato = Color4 (255/255) (99/255) (71/255) 1

orangeRed :: (Num a, Fractional a) => Color4 a
orangeRed = Color4 (255/255) (69/255) (0/255) 1

darkOrange :: (Num a, Fractional a) => Color4 a
darkOrange = Color4 (255/255) (140/255) (0/255) 1

gold :: (Num a, Fractional a) => Color4 a
gold = Color4 (255/255) (215/255) (0/255) 1

lightYellow :: (Num a, Fractional a) => Color4 a
lightYellow = Color4 (255/255) (255/255) (224/255) 1

lemonChiffon :: (Num a, Fractional a) => Color4 a
lemonChiffon = Color4 (255/255) (250/255) (205/255) 1

lightGoldenrodYellow :: (Num a, Fractional a) => Color4 a
lightGoldenrodYellow = Color4 (250/255) (250/255) (210/255) 1

papayaWhip :: (Num a, Fractional a) => Color4 a
papayaWhip = Color4 (255/255) (239/255) (213/255) 1

moccasin :: (Num a, Fractional a) => Color4 a
moccasin = Color4 (255/255) (228/255) (181/255) 1

peachPuff :: (Num a, Fractional a) => Color4 a
peachPuff = Color4 (255/255) (218/255) (185/255) 1

paleGoldenrod :: (Num a, Fractional a) => Color4 a
paleGoldenrod = Color4 (238/255) (232/255) (170/255) 1

khaki :: (Num a, Fractional a) => Color4 a
khaki = Color4 (240/255) (230/255) (140/255) 1

darkKhaki :: (Num a, Fractional a) => Color4 a
darkKhaki = Color4 (189/255) (183/255) (107/255) 1

lavender :: (Num a, Fractional a) => Color4 a
lavender = Color4 (230/255) (230/255) (250/255) 1

thistle :: (Num a, Fractional a) => Color4 a
thistle = Color4 (216/255) (191/255) (216/255) 1

plum :: (Num a, Fractional a) => Color4 a
plum = Color4 (221/255) (160/255) (221/255) 1

violet :: (Num a, Fractional a) => Color4 a
violet = Color4 (238/255) (130/255) (238/255) 1

orchid :: (Num a, Fractional a) => Color4 a
orchid = Color4 (218/255) (112/255) (214/255) 1

magenta :: (Num a, Fractional a) => Color4 a
magenta = Color4 (255/255) (0/255) (255/255) 1

mediumOrchid :: (Num a, Fractional a) => Color4 a
mediumOrchid = Color4 (186/255) (85/255) (211/255) 1

mediumPurple :: (Num a, Fractional a) => Color4 a
mediumPurple = Color4 (147/255) (112/255) (219/255) 1

amethyst :: (Num a, Fractional a) => Color4 a
amethyst = Color4 (153/255) (102/255) (204/255) 1

blueViolet :: (Num a, Fractional a) => Color4 a
blueViolet = Color4 (138/255) (43/255) (226/255) 1

darkViolet :: (Num a, Fractional a) => Color4 a
darkViolet = Color4 (148/255) (0/255) (211/255) 1

darkOrchid :: (Num a, Fractional a) => Color4 a
darkOrchid = Color4 (153/255) (50/255) (204/255) 1

darkMagenta :: (Num a, Fractional a) => Color4 a
darkMagenta = Color4 (139/255) (0/255) (139/255) 1

indigo :: (Num a, Fractional a) => Color4 a
indigo = Color4 (75/255) (0/255) (130/255) 1

slateBlue :: (Num a, Fractional a) => Color4 a
slateBlue = Color4 (106/255) (90/255) (205/255) 1

darkSlateBlue :: (Num a, Fractional a) => Color4 a
darkSlateBlue = Color4 (72/255) (61/255) (139/255) 1

mediumSlateBlue :: (Num a, Fractional a) => Color4 a
mediumSlateBlue = Color4 (123/255) (104/255) (238/255) 1

greenYellow :: (Num a, Fractional a) => Color4 a
greenYellow = Color4 (173/255) (255/255) (47/255) 1

chartreuse :: (Num a, Fractional a) => Color4 a
chartreuse = Color4 (127/255) (255/255) (0/255) 1

lawnGreen :: (Num a, Fractional a) => Color4 a
lawnGreen = Color4 (124/255) (252/255) (0/255) 1

limeGreen :: (Num a, Fractional a) => Color4 a
limeGreen = Color4 (50/255) (205/255) (50/255) 1

paleGreen :: (Num a, Fractional a) => Color4 a
paleGreen = Color4 (152/255) (251/255) (152/255) 1

lightGreen :: (Num a, Fractional a) => Color4 a
lightGreen = Color4 (144/255) (238/255) (144/255) 1

mediumSpringGreen :: (Num a, Fractional a) => Color4 a
mediumSpringGreen = Color4 0 (250/255) (154/255) 1

springGreen :: (Num a, Fractional a) => Color4 a
springGreen = Color4 0 (255/255) (127/255) 1

mediumSeaGreen :: (Num a, Fractional a) => Color4 a
mediumSeaGreen = Color4 (60/255) (179/255) (113/255) 1

seaGreen :: (Num a, Fractional a) => Color4 a
seaGreen = Color4 (46/255) (139/255) (87/255) 1

forestGreen :: (Num a, Fractional a) => Color4 a
forestGreen = Color4 (34/255) (139/255) (34/255) 1

darkGreen :: (Num a, Fractional a) => Color4 a
darkGreen = Color4 0 (100/255) (0/255) 1

yellowGreen :: (Num a, Fractional a) => Color4 a
yellowGreen = Color4 (154/255) (205/255) (50/255) 1

oliveDrab :: (Num a, Fractional a) => Color4 a
oliveDrab = Color4 (107/255) (142/255) (35/255) 1

darkOliveGreen :: (Num a, Fractional a) => Color4 a
darkOliveGreen = Color4 (85/255) (107/255) (47/255) 1

mediumAquamarine :: (Num a, Fractional a) => Color4 a
mediumAquamarine = Color4 (102/255) (205/255) (170/255) 1

darkSeaGreen :: (Num a, Fractional a) => Color4 a
darkSeaGreen = Color4 (143/255) (188/255) (143/255) 1

lightSeaGreen :: (Num a, Fractional a) => Color4 a
lightSeaGreen = Color4 (32/255) (178/255) (170/255) 1

darkCyan :: (Num a, Fractional a) => Color4 a
darkCyan = Color4 0 (139/255) (139/255) 1

cyan :: (Num a, Fractional a) => Color4 a
cyan = Color4 0 (255/255) (255/255) 1

lightCyan :: (Num a, Fractional a) => Color4 a
lightCyan = Color4 (224/255) (255/255) (255/255) 1

paleTurquoise :: (Num a, Fractional a) => Color4 a
paleTurquoise = Color4 (175/255) (238/255) (238/255) 1

aquamarine :: (Num a, Fractional a) => Color4 a
aquamarine = Color4 (127/255) (255/255) (212/255) 1

turquoise :: (Num a, Fractional a) => Color4 a
turquoise = Color4 (64/255) (224/255) (208/255) 1

mediumTurquoise :: (Num a, Fractional a) => Color4 a
mediumTurquoise = Color4 (72/255) (209/255) (204/255) 1

darkTurquoise :: (Num a, Fractional a) => Color4 a
darkTurquoise = Color4 0 (206/255) (209/255) 1

cadetBlue :: (Num a, Fractional a) => Color4 a
cadetBlue = Color4 (95/255) (158/255) (160/255) 1

steelBlue :: (Num a, Fractional a) => Color4 a
steelBlue = Color4 (70/255) (130/255) (180/255) 1

lightSteelBlue :: (Num a, Fractional a) => Color4 a
lightSteelBlue = Color4 (176/255) (196/255) (222/255) 1

powderBlue :: (Num a, Fractional a) => Color4 a
powderBlue = Color4 (176/255) (224/255) (230/255) 1

lightBlue :: (Num a, Fractional a) => Color4 a
lightBlue = Color4 (173/255) (216/255) (230/255) 1

skyBlue :: (Num a, Fractional a) => Color4 a
skyBlue = Color4 (135/255) (206/255) (235/255) 1

lightSkyBlue :: (Num a, Fractional a) => Color4 a
lightSkyBlue = Color4 (135/255) (206/255) (250/255) 1

deepSkyBlue :: (Num a, Fractional a) => Color4 a
deepSkyBlue = Color4 0 (191/255) (255/255) 1

dodgerBlue :: (Num a, Fractional a) => Color4 a
dodgerBlue = Color4 (30/255) (144/255) (255/255) 1

cornflowerBlue :: (Num a, Fractional a) => Color4 a
cornflowerBlue = Color4 (100/255) (149/255) (237/255) 1

royalBlue :: (Num a, Fractional a) => Color4 a
royalBlue = Color4 (65/255) (105/255) (225/255) 1

mediumBlue :: (Num a, Fractional a) => Color4 a
mediumBlue = Color4 0 (0/255) (205/255) 1

darkBlue :: (Num a, Fractional a) => Color4 a
darkBlue = Color4 0 (0/255) (139/255) 1

midnightBlue :: (Num a, Fractional a) => Color4 a
midnightBlue = Color4 (25/255) (25/255) (112/255) 1

cornsilk :: (Num a, Fractional a) => Color4 a
cornsilk = Color4 (255/255) (248/255) (220/255) 1

blanchedAlmond :: (Num a, Fractional a) => Color4 a
blanchedAlmond = Color4 (255/255) (235/255) (205/255) 1

bisque :: (Num a, Fractional a) => Color4 a
bisque = Color4 (255/255) (228/255) (196/255) 1

navajoWhite :: (Num a, Fractional a) => Color4 a
navajoWhite = Color4 (255/255) (222/255) (173/255) 1

wheat :: (Num a, Fractional a) => Color4 a
wheat = Color4 (245/255) (222/255) (179/255) 1

burlyWood :: (Num a, Fractional a) => Color4 a
burlyWood = Color4 (222/255) (184/255) (135/255) 1

tan :: (Num a, Fractional a) => Color4 a
tan = Color4 (210/255) (180/255) (140/255) 1

rosyBrown :: (Num a, Fractional a) => Color4 a
rosyBrown = Color4 (188/255) (143/255) (143/255) 1

sandyBrown :: (Num a, Fractional a) => Color4 a
sandyBrown = Color4 (244/255) (164/255) (96/255) 1

goldenrod :: (Num a, Fractional a) => Color4 a
goldenrod = Color4 (218/255) (165/255) (32/255) 1

darkGoldenrod :: (Num a, Fractional a) => Color4 a
darkGoldenrod = Color4 (184/255) (134/255) (11/255) 1

peru :: (Num a, Fractional a) => Color4 a
peru = Color4 (205/255) (133/255) (63/255) 1

chocolate :: (Num a, Fractional a) => Color4 a
chocolate = Color4 (210/255) (105/255) (30/255) 1

saddleBrown :: (Num a, Fractional a) => Color4 a
saddleBrown = Color4 (139/255) (69/255) (19/255) 1

sienna :: (Num a, Fractional a) => Color4 a
sienna = Color4 (160/255) (82/255) (45/255) 1

brown :: (Num a, Fractional a) => Color4 a
brown = Color4 (165/255) (42/255) (42/255) 1

snow :: (Num a, Fractional a) => Color4 a
snow = Color4 (255/255) (250/255) (250/255) 1

honeydew :: (Num a, Fractional a) => Color4 a
honeydew = Color4 (240/255) (255/255) (240/255) 1

mintCream :: (Num a, Fractional a) => Color4 a
mintCream = Color4 (245/255) (255/255) (250/255) 1

azure :: (Num a, Fractional a) => Color4 a
azure = Color4 (240/255) (255/255) (255/255) 1

aliceBlue :: (Num a, Fractional a) => Color4 a
aliceBlue = Color4 (240/255) (248/255) (255/255) 1

ghostWhite :: (Num a, Fractional a) => Color4 a
ghostWhite = Color4 (248/255) (248/255) (255/255) 1

whiteSmoke :: (Num a, Fractional a) => Color4 a
whiteSmoke = Color4 (245/255) (245/255) (245/255) 1

seashell :: (Num a, Fractional a) => Color4 a
seashell = Color4 (255/255) (245/255) (238/255) 1

beige :: (Num a, Fractional a) => Color4 a
beige = Color4 (245/255) (245/255) (220/255) 1

oldLace :: (Num a, Fractional a) => Color4 a
oldLace = Color4 (253/255) (245/255) (230/255) 1

floralWhite :: (Num a, Fractional a) => Color4 a
floralWhite = Color4 (255/255) (250/255) (240/255) 1

ivory :: (Num a, Fractional a) => Color4 a
ivory = Color4 (255/255) (255/255) (240/255) 1

antiqueWhite :: (Num a, Fractional a) => Color4 a
antiqueWhite = Color4 (250/255) (235/255) (215/255) 1

linen :: (Num a, Fractional a) => Color4 a
linen = Color4 (250/255) (240/255) (230/255) 1

lavenderBlush :: (Num a, Fractional a) => Color4 a
lavenderBlush = Color4 (255/255) (240/255) (245/255) 1

mistyRose :: (Num a, Fractional a) => Color4 a
mistyRose = Color4 (255/255) (228/255) (225/255) 1

gainsboro :: (Num a, Fractional a) => Color4 a
gainsboro = Color4 (220/255) (220/255) (220/255) 1

lightGrey :: (Num a, Fractional a) => Color4 a
lightGrey = Color4 (211/255) (211/255) (211/255) 1

darkGray :: (Num a, Fractional a) => Color4 a
darkGray = Color4 (169/255) (169/255) (169/255) 1

dimGray :: (Num a, Fractional a) => Color4 a
dimGray = Color4 (105/255) (105/255) (105/255) 1

lightSlateGray :: (Num a, Fractional a) => Color4 a
lightSlateGray = Color4 (119/255) (136/255) (153/255) 1

slateGray :: (Num a, Fractional a) => Color4 a
slateGray = Color4 (112/255) (128/255) (144/255) 1

darkSlateGray :: (Num a, Fractional a) => Color4 a
darkSlateGray = Color4 (47/255) (79/255) (79/255) 1

transparent :: (Num a, Fractional a) => Color4 a
transparent = Color4 0 0 0 0

alpha :: Fractional a => Color4 a -> a -> Color4 a
alpha (Color4 r g b _) a = Color4 r g b a

c42V4 :: Color4 a -> V4 a
c42V4 (Color4 r g b a) = V4 r g b a
