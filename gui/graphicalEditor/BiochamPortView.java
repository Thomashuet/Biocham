package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.CellView;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.PortView;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

public class BiochamPortView extends PortView {
	  
	  public BiochamPortView(Object port) {
	    super(port);
	    
	  }
	  
	  public Point2D getLocation(BiochamEdgeView edge, Point2D nearest) {
		 
		  
		  super.shouldInvokePortMagic(edge);
		  //shouldInvokePortMagic(edge);
	    CellView vertex = getParentView();
	    Point2D pos = null;
	    if (vertex != null) {
	      Rectangle2D r = vertex.getBounds();
	      double leftOrRight = r.getCenterX();
	  
	      if (vertex != null) {
	        DefaultGraphCell cell = (DefaultGraphCell) vertex.getCell();
	        String name = (String) cell.getUserObject();
	        if (name.equals("World")) {
	          leftOrRight = r.getMaxX();
	        } else if (name.equals("ess")) {
	          leftOrRight = r.getMinX();
	        }
	      }
	       
	      if (r != null)
	        return new Point2D.Double(leftOrRight, r.getCenterY());
	    }
	    return pos;
	  
	  }
}
 
	 
