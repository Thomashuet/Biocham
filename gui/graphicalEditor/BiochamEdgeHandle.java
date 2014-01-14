package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.CellView;
import org.jgraph.graph.EdgeView;
import org.jgraph.graph.GraphConstants;
import org.jgraph.graph.GraphContext;
import org.jgraph.graph.EdgeView.EdgeHandle;

import java.awt.event.MouseEvent;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Map;

public class BiochamEdgeHandle extends EdgeHandle {


	  protected boolean resetOffset = false;
	  
	  public BiochamEdgeHandle(EdgeView edge, GraphContext ctx) {
		  super (edge, ctx);
	  }
	  
	  public boolean isRemovePointEvent(MouseEvent event) {
		  return super .isRemovePointEvent(event) || event.isShiftDown();
	  }

	  public boolean isAddPointEvent(MouseEvent event) {
		  return super .isAddPointEvent(event) || event.isShiftDown();
	  }
	  
	  public void mousePressed(MouseEvent event) {
		  super .mousePressed(event);
		  if (isRemovePointEvent(event) && (source || target))
			  resetOffset = true;
	  }
	  
	  /**
      202:                 * Overrides the parent implementation to set the port offset if the
      203:                 * mousepointer is over the port's parent view but not over the port's
      204:                 * non-floating location (eg center).
      205:                 * 
      206:                 * @param isSource
      207:                 *            Whether to snap the source or target port.
      208:                 * @param point
      209:                 *            The point that should be used for snapping.
      210:                 */
      protected boolean snap(boolean isSource, Point2D point) {

          // Resets the offsets in the preview edge
          if (isSource)
              edge.getAllAttributes().remove(
                      JGraphpadGraphConstants.SOURCEPORTOFFSET);
          else
              edge.getAllAttributes().remove(
                      JGraphpadGraphConstants.TARGETPORTOFFSET);

          // Gets the bounds of the parent view and of the non-floating port
          Rectangle2D parent = null;
          Rectangle2D port = null;
          if (isSource && edge.getSource() != null) {
              parent = edge.getSource().getParentView().getBounds();
              port = edge.getSource().getBounds();
          } else if (target && edge.getTarget() != null) {
              parent = edge.getTarget().getParentView().getBounds();
              port = edge.getTarget().getBounds();
          }

          // Sets the port offset if the mousepointer is over the
         // source or target parent view bounds, but not over the
          // port itself.
          int tol = graph.getTolerance();
          if (port != null && port.contains(point)) {
              overlay(graph.getGraphics());
              edge.update(graph.getGraphLayoutCache());
            overlay(graph.getGraphics());
          } else if (parent != null
                  && parent
                          .intersects(new Rectangle2D.Double(point
                                  .getX()
                                  - tol, point.getY() - tol, 2 * tol,
                                  2 * tol))) {

              // Makes sure the port moving does not trigger on reconnects,
              // but only if the port is moved over the original source or
              // target.
              if ((isSource && edge.getSource() == orig.getSource())
                      || (target && edge.getTarget() == orig
                              .getTarget())) {
                  Point2D offset = computeOffset(point);
                  if (offset != null) {
                      edgeModified = true;
                      overlay(graph.getGraphics());
                      if (isSource)
                         JGraphpadGraphConstants
                                  .setSourcePortOffset(edge
                                          .getAllAttributes(), offset);
                      else
                          JGraphpadGraphConstants
                                  .setTargetPortOffset(edge
                                          .getAllAttributes(), offset);
                      edge.update(graph.getGraphLayoutCache());
                      overlay(graph.getGraphics());
                      resetOffset = false;
                  }
                  return true; // exit
              }
          }

          // Else the offset is reset and the superclass is called.
          resetOffset = true;
          return super .snap(isSource, point);
      }

      protected void processNestedMap(Map nested, boolean clone) {
    	  if (resetOffset) {
    		  Map attrs = (Map) nested.get(edge.getCell());
    		  if (attrs != null) {
    			  String[] removeAttributes = new String[] { (source) ? JGraphpadGraphConstants.SOURCEPORTOFFSET : JGraphpadGraphConstants.TARGETPORTOFFSET };
    			  GraphConstants.setRemoveAttributes(attrs,removeAttributes);
    		  }
    	  }
      }
      
      private Point2D computeOffset(Point2D pt) {
    	
    	  CellView portView = (source) ? edge.getSource() : edge.getTarget();
    	  
    	  if (portView != null) {
    		
    		  CellView vertex = portView.getParentView();
    		  pt = vertex.getPerimeterPoint(edge,EdgeView.getCenterPoint(vertex), pt);
    	  
    		  // Computes the relative vector for the perimeter point
    		  Rectangle2D rect = vertex.getBounds();
    		  pt = new Point2D.Double((pt.getX() - rect.getX()) / (rect.getWidth() - 1)* GraphConstants.PERMILLE, (pt.getY() - rect.getY())/ (rect.getHeight() - 1)* GraphConstants.PERMILLE);
    	  }
    	  return pt;
      }

      
}
