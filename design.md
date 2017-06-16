
what are the relevant information in a randomized pythagorean tree?
a) the size and position of the initial segment, relative to the screen size
b) the deviation between -12 and +12 from 45 degrees at each fork (or the size of the left branch of the fork relative to the size of the segment)

e.g. pos = (0,0)
     size = 1
     delta= 8
     alpha= (45 + delta) * pi / 180 

    top = (pos.x + size * cos alpha * cos alpha, pos.y + size + size * cos alpha * sin alpha)
    

    
