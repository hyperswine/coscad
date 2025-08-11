submersible uboatstyle example
low CoG, streamlined hull

length = 18cm
diameter = 5cm (r=2.5)

use coscad features to define the cross sections then use the hull operator
define 5 cross sections
The cross section looks kind of like

```

************ **********************
*                                 *
*                                 *
*                                 *
*                                 *
*                                 *
*                                 *
 *                               *
 *                               *
 *                               *
 *                               *
 *                               *
 *                               *
  *                             *
  *                             *
  **                           **
   **                          *
    **                        *
     **                     **
       **                **
        ******************
```

as you can see, flat on the top and bottom, but wider near the top. Circular on the sides.
One way to define is with a circle which we intersect a rectangle from.

```
                *****
               **** *
             ***  * **
*************************************************
**                                  **          *
 ***                              ***          **
   ***                          ***           **
     ***                       **           ***
       **************************************
```

The extra tower on the top is like a frustum unioned to the top flat surface of the submarine.

-------------

Bookend

```
     ******
     *    *
     *    *
     *    *
     *    *
     *    *
     *    *
     *    *
     *    *
     *    *
     *    *
    **    *
  ****    *
 **  *    *
 *   *    *
 *   *    *
 *   *    *
 *   *    *
 *   *    *
 *   *    *
 *   *    *
 *   *    *
 *   *    *
 *   *    *
**   *    *
*    *    *
*    *    *
*    *    *
*    *    *
*    *    ***********************
************************************
```
