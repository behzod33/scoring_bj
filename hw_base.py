import turtle
from random import random

# Настройка экрана
turtle.tracer(0)
turtle.bgcolor(0, 0, 0)
t = turtle.Turtle()
t.speed(0)
t.hideturtle()

# Основной цикл
for i in range(10000):
    t.clear()
    for j in range(80):
        # Установка случайного цвета
        t.color(random(), random(), random())
        # Рисование круга
        t.circle(150)
        # Поворот на 5 градусов
        t.left(5)
    # Обновление экрана
    turtle.update()
    t.home()
    t.left(i)
