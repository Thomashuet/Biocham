package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.CellViewRenderer;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.VertexRenderer;
import org.jgraph.graph.VertexView;

import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

public class IntermediateVertexNode2  extends VertexView{

	
	public IntermediateVertexRenderer2 renderer=new IntermediateVertexRenderer2();
	private DefaultGraphCell parentCell;
	BiochamEdgeData userObject;
	
	public IntermediateVertexNode2() {
		super();
	}

	/**
	 */
	public IntermediateVertexNode2(Object cell) {
		super(cell);
		parentCell=(DefaultGraphCell)cell;
		
	}
		
	public CellViewRenderer getRenderer() {
		return renderer;
	}
	
	
	public class IntermediateVertexRenderer2 extends VertexRenderer { //implements MouseListener{
		
	
		public void paint(Graphics g) {
	
			
			int b = borderWidth;
			Graphics2D g2 = (Graphics2D) g;
			Dimension d = getSize();
			setMaximumSize(d);	
			g2.setPaint(Color.BLACK);
			g.drawLine(0,d.height/2, 1, d.height/2);
			
			Utils.debugMsg(parentCell.getClass().toString());
			Rectangle2D bnds=BiochamGraphConstants.getBounds(((DefaultGraphCell)cell).getAttributes());
			if(bnds!=null){
				
				
				double xx=bnds.getX();
				double yy=bnds.getY();
				IntermediateVertex2 iv=(IntermediateVertex2)parentCell;
				iv.setX(xx);
				iv.setY(yy);
				
			}
					
		}
		
		
		public Point2D getPerimeterPoint(VertexView view, Point2D source, Point2D p) {
			Rectangle2D bounds = view.getBounds();
			double x = bounds.getX();
			double y = bounds.getY();
			double width = bounds.getWidth();
			double height = bounds.getHeight();
			double xCenter = x + width / 2;
			double yCenter = y + height / 2;
			double dx = p.getX() - xCenter; // Compute Angle
			double dy = p.getY() - yCenter;
			double alpha = Math.atan2(dy, dx);
			double xout = 0, yout = 0;
			double pi = Math.PI;
			double pi2 = Math.PI / 2.0;
			double beta = pi2 - alpha;
			double t = Math.atan2(height, width);
			if (alpha < -pi + t || alpha > pi - t) { // Left edge
				xout = x;
				yout = yCenter - width * Math.tan(alpha) / 2;
			} else if (alpha < -t) { // Top Edge
				yout = y;
				xout = xCenter - height * Math.tan(beta) / 2;
			} else if (alpha < t) { // Right Edge
				xout = x + width;
				yout = yCenter + width * Math.tan(alpha) / 2;
			} else { // Bottom Edge
				yout = y + height;
				xout = xCenter + height * Math.tan(beta) / 2;
			}
			return new Point2D.Double(xout, yout);
		}
	}
	
	
}
