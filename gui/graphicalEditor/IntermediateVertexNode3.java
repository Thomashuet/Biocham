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
import java.awt.geom.Rectangle2D;

public class IntermediateVertexNode3 extends VertexView{

	
	public IntermediateVertexRenderer3 renderer=new IntermediateVertexRenderer3();
	private DefaultGraphCell parentCell;
	BiochamEdgeData userObject;
	
	public IntermediateVertexNode3() {
		super();
	}

	/**
	 */
	public IntermediateVertexNode3(Object cell) {
		super(cell);
		parentCell=(DefaultGraphCell)cell;
	}
		
	public CellViewRenderer getRenderer() {
		return renderer;
	}
	
	
	public class IntermediateVertexRenderer3 extends VertexRenderer { 
		
	
		public void paint(Graphics g) {
	
			
		
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
				IntermediateVertex3 iv=(IntermediateVertex3)parentCell;
				iv.setX(xx);
				iv.setY(yy);
				
			}
						
		}
		
		
		
	}
	
	
}

