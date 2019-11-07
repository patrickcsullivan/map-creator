# Map Creator

This is a work-in-progress tool for creating maps for use in agent based modeling simulations.

A map consists of multiple layers. Each layer is a grid, and all layers have the same width and height. Every cell on a layer's grid has an integer value associated with it, and each layer has a min value and a max value that bound the possible values of the cells. Every layer also has a color gradient that determines how each cell in the layer's grid is colored.

You can paint values onto the map by clicking and dragging on it. Maps can be saved and loaded as JSON.

![painting](https://raw.githubusercontent.com/patrickcsullivan/map-creator/view/doc/painting.gif "painting")

![editing a gradient](https://raw.githubusercontent.com/patrickcsullivan/map-creator/view/doc/gradient.gif "editing a gradient")

