package fr.inria.contraintes.biocham.graphicalEditor;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.jgraph.graph.AbstractCellView;
import org.jgraph.graph.CellView;
import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.DefaultPort;
import org.jgraph.graph.EdgeView;
import org.jgraph.graph.GraphLayoutCache;
import org.jgraph.util.ParallelEdgeRouter;

import fr.inria.contraintes.biocham.utils.Utils;

/**
* Class that manage the parallel routing and the self loop routing.
*/
public class MyParallelEdgeRouter extends ParallelEdgeRouter {
/**
* Serial version UID.
*/
private static final long serialVersionUID = -6896434368278275922L;

/**
* Singleton to reach parallel edge router.
*/
protected static final MyParallelEdgeRouter sharedInstance =
new MyParallelEdgeRouter();

/**
* Set the max number of loops.
*/
public static final int maxSelfLoop = 4;

/**
* Getter for singleton managing parallel edges.
* @return MyParallelEdgeRouter for parallel edges
*/
public static MyParallelEdgeRouter getSharedInstance() {
return MyParallelEdgeRouter.sharedInstance;
}

/**
* Define the route for a self loop.
* @param cache The GraphLayoutCache object of the graph
* @param edge The edge to route
* @return A list of points and EdgeView
*/
@Override
protected List routeLoop(GraphLayoutCache cache, EdgeView edge) {
List newPoints = new ArrayList();
CellView sourceParent =
(edge.getSource() != null) ? edge.getSource().getParentView()
: edge.getSourceParentView();
if (sourceParent != null) {
int position = getLoopNumber(edge, sourceParent);
Point2D from = AbstractCellView.getCenterPoint(sourceParent);
Rectangle2D rect = sourceParent.getBounds();
double width = rect.getWidth();
double height2 = (rect.getHeight() / 2);
double loopWidth = Math.min(20, Math.max(10, width / 8));
double loopHeight =
Math.min(30, Math.max(12, Math.max(loopWidth + 4,
height2 / 2)));
newPoints.add(edge.getSource());
initPoints(rect, edge, newPoints, from, height2, loopWidth,
loopHeight, position);
newPoints.add(edge.getTarget());
return newPoints;

/* Second method, infinity number of loop but less beautiful
CellView view = edge.getSource(); double x =
view.getBounds().getX(); double y = view.getBounds().getY();
Point2D controlPoint0 = new Point2D.Double(x - (20+position *
20), y); Point2D controlPoint1 = new Point2D.Double(x -
(20+position * 20), y - (20+position * 20)); Point2D
controlPoint2 = new Point2D.Double(x , y - (40+position * 20));
Point2D controlPoint3 = new Point2D.Double(x + (20+position *
20), y - (20+position * 20)); Point2D controlPoint4 = new
Point2D.Double(x + (20+position * 20), y - (10+position*20));
newPoints.add(controlPoint0); newPoints.add(controlPoint1);
newPoints.add(controlPoint2); newPoints.add(controlPoint3);
newPoints.add(controlPoint4); return newPoints;
*/

}
Utils.debugMsg("never reached");
return null;
}

/**
* Set the self loop points.
*
* @param rect the vertex rectangle.
* @param edge the edge.
* @param newPoints the point array.
* @param from center vertex point.
* @param height2 vertex height divide by 2.
* @param loopWidth loop width.
* @param loopHeight loop height.
* @param factor the factor where to draw the points.
*/
private void initPoints(Rectangle2D rect, EdgeView edge,
List < Point2D > newPoints, Point2D from, double height2,
double loopWidth, double loopHeight, int factor) {

switch (factor) {
case 0: { // Top loop.
newPoints.add(edge.getAttributes().createPoint(
from.getX() - loopWidth,
from.getY() - height2 - loopHeight * 1.2));
newPoints.add(edge.getAttributes().createPoint(from.getX(),
from.getY() - height2 - 1.5 * loopHeight));
newPoints.add(edge.getAttributes().createPoint(
from.getX() + loopWidth,
from.getY() - height2 - loopHeight * 1.2));
break;
}

case 1: { // right side loop.

double width2 = (rect.getWidth() / 2);
newPoints.add(edge.getAttributes().createPoint(
from.getX() + width2 + loopHeight * 1.2,
from.getY() - loopWidth));
newPoints.add(edge.getAttributes().createPoint(
from.getX() + width2 + loopHeight * 1.5, from.getY()));
newPoints.add(edge.getAttributes().createPoint(
from.getX() + width2 + loopHeight * 1.2,
from.getY() + loopWidth));
break;
}

case 2: { // Bottom loop.
newPoints.add(edge.getAttributes().createPoint(
from.getX() - loopWidth,
from.getY() + height2 + loopHeight * 1.2));
newPoints.add(edge.getAttributes().createPoint(from.getX(),
from.getY() + height2 + 1.5 * loopHeight));
newPoints.add(edge.getAttributes().createPoint(
from.getX() + loopWidth,
from.getY() + height2 + loopHeight * 1.2));
break;
}

case 3: { // Left side loop.
double width2 = (rect.getWidth() / 2);
newPoints.add(edge.getAttributes().createPoint(
from.getX() - width2 - loopHeight * 1.2,
from.getY() - loopWidth));
newPoints.add(edge.getAttributes().createPoint(
from.getX() - width2 - loopHeight * 1.5, from.getY()));
newPoints.add(edge.getAttributes().createPoint(
from.getX() - width2 - loopHeight * 1.2,
from.getY() + loopWidth));
break;
}
}
}

/**
* Get the position of the current edge.
* @param edge the current edge to route
* @param sourceParent the parent CellView
* @return the position of the current edge
*/
private int getLoopNumber(EdgeView edge, CellView sourceParent) {
// Test whether the CellView has already self loops.
DefaultGraphCell cell = (DefaultGraphCell) sourceParent.getCell();
DefaultPort port = (DefaultPort) cell.getChildAt(0);
DefaultEdge edgeCell = (DefaultEdge) edge.getCell();

int factor = 0;
for (Iterator < DefaultEdge > iter = port.edges(); iter.hasNext();) {
DefaultEdge curEdge = iter.next();
if (curEdge.equals(edgeCell)) {
break;
}
if(curEdge.getSource().equals(curEdge.getTarget())) {
factor++;
}
}
return factor;
}
}
