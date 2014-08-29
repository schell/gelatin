module Gelatin.Color where

import Gelatin.Core
import Linear

maroon :: Color
maroon = V4 (128/255) (0/255) (0/255) 1

red :: Color
red = V4 (255/255) (0/255) (0/255) 1

orange :: Color
orange = V4 (255/255) (165/255) (0/255) 1

yellow :: Color
yellow = V4 (255/255) (255/255) (0/255) 1

olive :: Color
olive = V4 (128/255) (128/255) (0/255) 1

green :: Color
green = V4 0 (128/255) (0/255) 1

purple :: Color
purple = V4 (128/255) (0/255) (128/255) 1

fuchsia :: Color
fuchsia = V4 (255/255) (0/255) (255/255) 1

lime :: Color
lime = V4 0 (255/255) (0/255) 1

teal :: Color
teal = V4 0 (128/255) (128/255) 1

aqua :: Color
aqua = V4 0 (255/255) (255/255) 1

blue :: Color
blue = V4 0 (0/255) (255/255) 1

navy :: Color
navy = V4 0 (0/255) (128/255) 1

black :: Color
black = V4 0 (0/255) (0/255) 1

gray :: Color
gray = V4 (128/255) (128/255) (128/255) 1

silver :: Color
silver = V4 (192/255) (192/255) (192/255) 1

white :: Color
white = V4 (255/255) (255/255) (255/255) 1

indianRed :: Color
indianRed = V4 (205/255) (92/255) (92/255) 1

lightCoral :: Color
lightCoral = V4 (240/255) (128/255) (128/255) 1

salmon :: Color
salmon = V4 (250/255) (128/255) (114/255) 1

darkSalmon :: Color
darkSalmon = V4 (233/255) (150/255) (122/255) 1

lightSalmon :: Color
lightSalmon = V4 (255/255) (160/255) (122/255) 1

crimson :: Color
crimson = V4 (220/255) (20/255) (60/255) 1

fireBrick :: Color
fireBrick = V4 (178/255) (34/255) (34/255) 1

darkRed :: Color
darkRed = V4 (139/255) (0/255) (0/255) 1

pink :: Color
pink = V4 (255/255) (192/255) (203/255) 1

lightPink :: Color
lightPink = V4 (255/255) (182/255) (193/255) 1

hotPink :: Color
hotPink = V4 (255/255) (105/255) (180/255) 1

deepPink :: Color
deepPink = V4 (255/255) (20/255) (147/255) 1

mediumVioletRed :: Color
mediumVioletRed = V4 (199/255) (21/255) (133/255) 1

paleVioletRed :: Color
paleVioletRed = V4 (219/255) (112/255) (147/255) 1

coral :: Color
coral = V4 (255/255) (127/255) (80/255) 1

tomato :: Color
tomato = V4 (255/255) (99/255) (71/255) 1

orangeRed :: Color
orangeRed = V4 (255/255) (69/255) (0/255) 1

darkOrange :: Color
darkOrange = V4 (255/255) (140/255) (0/255) 1

gold :: Color
gold = V4 (255/255) (215/255) (0/255) 1

lightYellow :: Color
lightYellow = V4 (255/255) (255/255) (224/255) 1

lemonChiffon :: Color
lemonChiffon = V4 (255/255) (250/255) (205/255) 1

lightGoldenrodYellow :: Color
lightGoldenrodYellow = V4 (250/255) (250/255) (210/255) 1

papayaWhip :: Color
papayaWhip = V4 (255/255) (239/255) (213/255) 1

moccasin :: Color
moccasin = V4 (255/255) (228/255) (181/255) 1

peachPuff :: Color
peachPuff = V4 (255/255) (218/255) (185/255) 1

paleGoldenrod :: Color
paleGoldenrod = V4 (238/255) (232/255) (170/255) 1

khaki :: Color
khaki = V4 (240/255) (230/255) (140/255) 1

darkKhaki :: Color
darkKhaki = V4 (189/255) (183/255) (107/255) 1

lavender :: Color
lavender = V4 (230/255) (230/255) (250/255) 1

thistle :: Color
thistle = V4 (216/255) (191/255) (216/255) 1

plum :: Color
plum = V4 (221/255) (160/255) (221/255) 1

violet :: Color
violet = V4 (238/255) (130/255) (238/255) 1

orchid :: Color
orchid = V4 (218/255) (112/255) (214/255) 1

magenta :: Color
magenta = V4 (255/255) (0/255) (255/255) 1

mediumOrchid :: Color
mediumOrchid = V4 (186/255) (85/255) (211/255) 1

mediumPurple :: Color
mediumPurple = V4 (147/255) (112/255) (219/255) 1

amethyst :: Color
amethyst = V4 (153/255) (102/255) (204/255) 1

blueViolet :: Color
blueViolet = V4 (138/255) (43/255) (226/255) 1

darkViolet :: Color
darkViolet = V4 (148/255) (0/255) (211/255) 1

darkOrchid :: Color
darkOrchid = V4 (153/255) (50/255) (204/255) 1

darkMagenta :: Color
darkMagenta = V4 (139/255) (0/255) (139/255) 1

indigo :: Color
indigo = V4 (75/255) (0/255) (130/255) 1

slateBlue :: Color
slateBlue = V4 (106/255) (90/255) (205/255) 1

darkSlateBlue :: Color
darkSlateBlue = V4 (72/255) (61/255) (139/255) 1

mediumSlateBlue :: Color
mediumSlateBlue = V4 (123/255) (104/255) (238/255) 1

greenYellow :: Color
greenYellow = V4 (173/255) (255/255) (47/255) 1

chartreuse :: Color
chartreuse = V4 (127/255) (255/255) (0/255) 1

lawnGreen :: Color
lawnGreen = V4 (124/255) (252/255) (0/255) 1

limeGreen :: Color
limeGreen = V4 (50/255) (205/255) (50/255) 1

paleGreen :: Color
paleGreen = V4 (152/255) (251/255) (152/255) 1

lightGreen :: Color
lightGreen = V4 (144/255) (238/255) (144/255) 1

mediumSpringGreen :: Color
mediumSpringGreen = V4 0 (250/255) (154/255) 1

springGreen :: Color
springGreen = V4 0 (255/255) (127/255) 1

mediumSeaGreen :: Color
mediumSeaGreen = V4 (60/255) (179/255) (113/255) 1

seaGreen :: Color
seaGreen = V4 (46/255) (139/255) (87/255) 1

forestGreen :: Color
forestGreen = V4 (34/255) (139/255) (34/255) 1

darkGreen :: Color
darkGreen = V4 0 (100/255) (0/255) 1

yellowGreen :: Color
yellowGreen = V4 (154/255) (205/255) (50/255) 1

oliveDrab :: Color
oliveDrab = V4 (107/255) (142/255) (35/255) 1

darkOliveGreen :: Color
darkOliveGreen = V4 (85/255) (107/255) (47/255) 1

mediumAquamarine :: Color
mediumAquamarine = V4 (102/255) (205/255) (170/255) 1

darkSeaGreen :: Color
darkSeaGreen = V4 (143/255) (188/255) (143/255) 1

lightSeaGreen :: Color
lightSeaGreen = V4 (32/255) (178/255) (170/255) 1

darkCyan :: Color
darkCyan = V4 0 (139/255) (139/255) 1

cyan :: Color
cyan = V4 0 (255/255) (255/255) 1

lightCyan :: Color
lightCyan = V4 (224/255) (255/255) (255/255) 1

paleTurquoise :: Color
paleTurquoise = V4 (175/255) (238/255) (238/255) 1

aquamarine :: Color
aquamarine = V4 (127/255) (255/255) (212/255) 1

turquoise :: Color
turquoise = V4 (64/255) (224/255) (208/255) 1

mediumTurquoise :: Color
mediumTurquoise = V4 (72/255) (209/255) (204/255) 1

darkTurquoise :: Color
darkTurquoise = V4 0 (206/255) (209/255) 1

cadetBlue :: Color
cadetBlue = V4 (95/255) (158/255) (160/255) 1

steelBlue :: Color
steelBlue = V4 (70/255) (130/255) (180/255) 1

lightSteelBlue :: Color
lightSteelBlue = V4 (176/255) (196/255) (222/255) 1

powderBlue :: Color
powderBlue = V4 (176/255) (224/255) (230/255) 1

lightBlue :: Color
lightBlue = V4 (173/255) (216/255) (230/255) 1

skyBlue :: Color
skyBlue = V4 (135/255) (206/255) (235/255) 1

lightSkyBlue :: Color
lightSkyBlue = V4 (135/255) (206/255) (250/255) 1

deepSkyBlue :: Color
deepSkyBlue = V4 0 (191/255) (255/255) 1

dodgerBlue :: Color
dodgerBlue = V4 (30/255) (144/255) (255/255) 1

cornflowerBlue :: Color
cornflowerBlue = V4 (100/255) (149/255) (237/255) 1

royalBlue :: Color
royalBlue = V4 (65/255) (105/255) (225/255) 1

mediumBlue :: Color
mediumBlue = V4 0 (0/255) (205/255) 1

darkBlue :: Color
darkBlue = V4 0 (0/255) (139/255) 1

midnightBlue :: Color
midnightBlue = V4 (25/255) (25/255) (112/255) 1

cornsilk :: Color
cornsilk = V4 (255/255) (248/255) (220/255) 1

blanchedAlmond :: Color
blanchedAlmond = V4 (255/255) (235/255) (205/255) 1

bisque :: Color
bisque = V4 (255/255) (228/255) (196/255) 1

navajoWhite :: Color
navajoWhite = V4 (255/255) (222/255) (173/255) 1

wheat :: Color
wheat = V4 (245/255) (222/255) (179/255) 1

burlyWood :: Color
burlyWood = V4 (222/255) (184/255) (135/255) 1

tan :: Color
tan = V4 (210/255) (180/255) (140/255) 1

rosyBrown :: Color
rosyBrown = V4 (188/255) (143/255) (143/255) 1

sandyBrown :: Color
sandyBrown = V4 (244/255) (164/255) (96/255) 1

goldenrod :: Color
goldenrod = V4 (218/255) (165/255) (32/255) 1

darkGoldenrod :: Color
darkGoldenrod = V4 (184/255) (134/255) (11/255) 1

peru :: Color
peru = V4 (205/255) (133/255) (63/255) 1

chocolate :: Color
chocolate = V4 (210/255) (105/255) (30/255) 1

saddleBrown :: Color
saddleBrown = V4 (139/255) (69/255) (19/255) 1

sienna :: Color
sienna = V4 (160/255) (82/255) (45/255) 1

brown :: Color
brown = V4 (165/255) (42/255) (42/255) 1

snow :: Color
snow = V4 (255/255) (250/255) (250/255) 1

honeydew :: Color
honeydew = V4 (240/255) (255/255) (240/255) 1

mintCream :: Color
mintCream = V4 (245/255) (255/255) (250/255) 1

azure :: Color
azure = V4 (240/255) (255/255) (255/255) 1

aliceBlue :: Color
aliceBlue = V4 (240/255) (248/255) (255/255) 1

ghostWhite :: Color
ghostWhite = V4 (248/255) (248/255) (255/255) 1

whiteSmoke :: Color
whiteSmoke = V4 (245/255) (245/255) (245/255) 1

seashell :: Color
seashell = V4 (255/255) (245/255) (238/255) 1

beige :: Color
beige = V4 (245/255) (245/255) (220/255) 1

oldLace :: Color
oldLace = V4 (253/255) (245/255) (230/255) 1

floralWhite :: Color
floralWhite = V4 (255/255) (250/255) (240/255) 1

ivory :: Color
ivory = V4 (255/255) (255/255) (240/255) 1

antiqueWhite :: Color
antiqueWhite = V4 (250/255) (235/255) (215/255) 1

linen :: Color
linen = V4 (250/255) (240/255) (230/255) 1

lavenderBlush :: Color
lavenderBlush = V4 (255/255) (240/255) (245/255) 1

mistyRose :: Color
mistyRose = V4 (255/255) (228/255) (225/255) 1

gainsboro :: Color
gainsboro = V4 (220/255) (220/255) (220/255) 1

lightGrey :: Color
lightGrey = V4 (211/255) (211/255) (211/255) 1

darkGray :: Color
darkGray = V4 (169/255) (169/255) (169/255) 1

dimGray :: Color
dimGray = V4 (105/255) (105/255) (105/255) 1

lightSlateGray :: Color
lightSlateGray = V4 (119/255) (136/255) (153/255) 1

slateGray :: Color
slateGray = V4 (112/255) (128/255) (144/255) 1

darkSlateGray :: Color
darkSlateGray = V4 (47/255) (79/255) (79/255) 1

transparent :: Color
transparent = V4 0 0 0 0

alpha :: Fractional a => V4 a -> a -> V4 a
alpha (V4 r g b _) a = V4 r g b a
