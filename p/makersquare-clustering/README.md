# Clustering

We will implement a clustering algorithm in JavaScript. Provided for you is a canvas where you can draw points and call your clustering methods on. All the drawing has been implemented for you (but feel free to modify). You simply have to implement `lib/clustering.js`.

**data/** contains two data files that you can use to load either the world at night or the United States at night.

**images/** contains the original images and scripts I used to convert the images to data points.

You shouldn't need to modify **js/app.js** and **js/canvas.js**, as these are the files that draw the canvas for you. You'll need to modify them if you do the project extensions, however.

# Clustering

The `Clustering` class in `js/clustering.js` contain only a handful of helper methods
and starting instance variables. See how `Clustering` is used in `js/canvas.js` to
know what the unimplemented methods in `Clustering` should do.

# Extensions (optional)

**Cost function**

A clustering algorithm may converge to different solutions. Some of them are better than others. We can mathematically define how good a solution is by using a *cost function*. The cost function for a clustering algorithm is the sum of the distance between each point and their designated center.

Calculate and display the cost somewhere in the canvas.

**Minimize cost**

Build a runner that will run your algorithm multiple times, and choose the best solution (i.e. select the solution that minimizes the cost).

**Selecting cluster size**

Build a feature that will intelligently and automatically select the starting number of clusters for you using the "elbow" method as described in the lecture.

**Multi-dimensional Clustering**

We implemented a two-dimensional algorithm because it's easy to visualize. But in theory, you can cluster on data points of any dimension. Change your `Clustering` to support arbitrary dimension sizes.

