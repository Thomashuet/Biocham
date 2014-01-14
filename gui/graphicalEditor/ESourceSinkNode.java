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


public class ESourceSinkNode extends VertexView{

	
public SourceSinkRenderer renderer=new SourceSinkRenderer();
	
	DefaultGraphCell cell;
	
	public ESourceSinkNode() {
		super();
	}

	/**
	 */
	public ESourceSinkNode(Object c) {
		super(c);
		if(c instanceof DefaultGraphCell){
			cell=(DefaultGraphCell)c;
		}
		
	}
		
	public CellViewRenderer getRenderer() {
		return renderer;
	}
	
	public String toString(){ return "";}
	
	public class SourceSinkRenderer extends VertexRenderer {
		
		
		public Dimension getPreferredSize() {
			Dimension d = super.getPreferredSize();
			d.width += d.width / 8;
			d.height += d.height/2 ;
			return d;
		}
		
		public void paint(Graphics g) {
			
			
			setDoubleBuffered(true);
			setOpaque(false);
			
			int b = borderWidth;
			Graphics2D g2 = (Graphics2D) g;
			Dimension d = getSize();
			boolean tmp = selected;	
			g.setColor(super.getBackground());
			gradientColor=Color.BLACK;
			if (gradientColor != null && !preview) {
				setOpaque(true);
				g2.setPaint(Color.black);
				g.drawOval(b/2,b/2,d.height-1,d.height-1);
				g.setColor(Color.black);
				g.drawLine(b/2,b/2+d.height,b/2+d.height,b/2);
				
			}
			try {
				setBorder(null);
				setOpaque(false);
				selected = false;
				super.paint(g);
			} finally {
				selected = tmp;
			}
			if(cell!=null){
				Rectangle2D bnds=BiochamGraphConstants.getBounds(((DefaultGraphCell)cell).getAttributes());
				if(bnds!=null){
										
					double xx=bnds.getX();
					double yy=bnds.getY();
					ESourceSink iv=(ESourceSink)cell;
					iv.setX(xx);
					iv.setY(yy);
					if(cell.getUserObject()!=null){
						BiochamEntityData dt=(BiochamEntityData)cell.getUserObject();
						if(dt.getPosition()!=null){
							dt.getPosition().setX(xx);
							dt.getPosition().setY(yy);
						}
					}
					
				}
			}
			
			                                
		}
	}
	
}
