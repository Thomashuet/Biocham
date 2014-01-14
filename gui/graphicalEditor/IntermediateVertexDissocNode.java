package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.CellViewRenderer;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.VertexRenderer;
import org.jgraph.graph.VertexView;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;




public class IntermediateVertexDissocNode extends VertexView{

	
	public IntermediateVertexDissocRenderer renderer=new IntermediateVertexDissocRenderer();
	private DefaultGraphCell parentCell;
	BiochamEdgeData userObject;
	Rectangle2D bounds;
	
	
	
	public IntermediateVertexDissocNode(Object cell) {
		super(cell);
		if(cell instanceof IntermediateVertexDissoc){
			parentCell=(IntermediateVertexDissoc)cell;
			
			if(parentCell!=null){
				double x,y;
				x=0;
				y=0;
				bounds=BiochamGraphConstants.getBounds(parentCell.getAttributes());
				if(bounds!=null){
					x=bounds.getX();
					y=bounds.getY();
					
				}
				if(((IntermediateVertexDissoc)parentCell).getUserObject() instanceof BiochamEdgeData){
					userObject=(BiochamEdgeData)((IntermediateVertexDissoc)parentCell).getUserObject();
					if(userObject!=null){
						if(userObject.getPosition()==null){
							userObject.setPosition(new Position(0,0,x,y));
						}else{
							userObject.getPosition().setX1(x);
							userObject.getPosition().setY1(y);
						}	
					}
				}
			}
			
		}

	}
		
	public CellViewRenderer getRenderer() {
		return renderer;
	}

	public class IntermediateVertexDissocRenderer extends VertexRenderer { 
		
		
		public void paint(Graphics g) {
	
			
			int b = borderWidth;
			Graphics2D g2 = (Graphics2D) g;
			Dimension d = getSize();
			setMaximumSize(d);
		
			if(userObject.getColor()!=null){
				g2.setPaint(userObject.getColor());
			}else{
				g2.setPaint(Color.BLACK);
			}
			g.drawOval(3,3,3,3 );
			g.drawOval(0,0,9,9 );
		
			
			if(userObject!=null){
				Position pos=userObject.getPosition();
			
				if(pos!=null){
				
					Rectangle2D bnds=BiochamGraphConstants.getBounds(((DefaultGraphCell)cell).getAttributes());
					if(bnds!=null){
						double xx=bnds.getX();
						double yy=bnds.getY();
					
						IntermediateVertexDissoc iv=(IntermediateVertexDissoc)parentCell;
						iv.setX(xx);
						iv.setY(yy);
						pos.setX1(xx);
						pos.setY1(yy);
					}
				}
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
		}
	}
}
