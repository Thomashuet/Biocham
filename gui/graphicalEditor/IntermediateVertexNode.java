package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.CellViewRenderer;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.VertexRenderer;
import org.jgraph.graph.VertexView;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;




public class IntermediateVertexNode  extends VertexView{

	
	public IntermediateVertexRenderer renderer=new IntermediateVertexRenderer();
	private DefaultGraphCell parentCell;
	BiochamEdgeData userObject;
	Rectangle2D bounds;
	
	public IntermediateVertexNode(Object cell) {
		
		super(cell);
		
		if(cell instanceof DefaultGraphCell){
		
			parentCell=(DefaultGraphCell)cell;			
			if(parentCell!=null){
				
				double x=0,y=0;				
				bounds=BiochamGraphConstants.getBounds(parentCell.getAttributes());			
				if(bounds!=null){
					x=bounds.getX();
					y=bounds.getY();
				}				
				userObject=(BiochamEdgeData)parentCell.getUserObject();
				
				if(userObject.getPosition()==null){				
					userObject.setPosition(new Position(x,y,0,0));					
				}	
				userObject.setDrawingInstance(this);
			}	
		}
	}
		
	public CellViewRenderer getRenderer() {
		return renderer;
	}
	
	
	public class IntermediateVertexRenderer extends VertexRenderer { 
		
	
		public void paint(Graphics g) {
				
			Graphics2D g2 = (Graphics2D) g;
			if(userObject.getColor()!=null){
				
				g2.setPaint(userObject.getColor());
				
			}else{	
				
				g2.setPaint(Color.BLACK);
			}
		
			
			Position pos=userObject.getPosition();
			
			if(pos!=null){
				
				Rectangle2D bnds=BiochamGraphConstants.getBounds(((DefaultGraphCell)cell).getAttributes());
				if(bnds!=null){
					
					double xc=pos.getX();
					double yc=pos.getY();
					double xx=bnds.getX();
					double yy=bnds.getY();
					IntermediateVertex iv=(IntermediateVertex)parentCell;
					iv.setX(xx);
					iv.setY(yy);
					pos.setX(xx);
					pos.setY(yy);
				}
			}			
			g.drawRect(0,0,IntermediateVertex.WIDTH-1,IntermediateVertex.HEIGHT-1);			
			if(userObject instanceof BiochamEdgeData){
			
				if(userObject.getGraph().isShowReactionKinetics()){
					
					if(userObject!=null){
						
						if(userObject.getKinetics()!="" && userObject.getKinetics()!=null){
							
							int k=g.getFontMetrics().stringWidth(userObject.getKinetics())/2;
							g.setClip(-50,-50,400,400);
							g.drawString(userObject.getKinetics(), -k, -g.getFontMetrics().getHeight()/4);
							
						}
					}
				}
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


	public DefaultGraphCell getParentCell() {
		return parentCell;
	}

	public void setParentCell(DefaultGraphCell parentCell) {
		this.parentCell = parentCell;
	}
	
	
}
