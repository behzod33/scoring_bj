from turtle import *
from random import random

setup(1.0, 1.0)
shape("turtle")
tracer(0)
bgcolor(0, 0, 0)

rows, cols = 10, 8
cell_size = 30

penup()
goto(-cols * cell_size / 2, rows * cell_size / 2)

for i in range(rows):
    for j in range(cols):
        color(random(), random(), random())
        down()
        forward(cell_size)
        right(90)
        forward(cell_size)
        right(90)
        forward(cell_size)
        right(90)
        forward(cell_size)
        right(90)
        penup()
        forward(cell_size)
    goto(-cols * cell_size / 2, rows * cell_size / 2 - (i + 1) * cell_size)

update()
done()
