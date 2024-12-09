from turtle import *
from random import random

setup(1.0, 1.0)
shape("turtle")
tracer(0)
bgcolor(0, 0, 0)

cell_size = 30
start_x = -300
start_y = 300

penup()

for i in range(10):
    for j in range(8):
        goto(start_x + j * cell_size, start_y - i * cell_size)
        
        color(random(), random(), random())
        pendown()
        
        goto(start_x + (j + 1) * cell_size, start_y - i * cell_size)
        goto(start_x + (j + 1) * cell_size, start_y - (i + 1) * cell_size)
        goto(start_x + j * cell_size, start_y - (i + 1) * cell_size)
        goto(start_x + j * cell_size, start_y - i * cell_size)
        
        penup()

update()
done()
