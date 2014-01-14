package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultGraphCell;

import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.geom.Rectangle2D;
import java.util.Hashtable;
import java.util.Map;

public class IntermediateVertex2 extends DefaultGraphCell {

	
	
	public static int WIDTH=1;
	public static int HEIGHT=0;	
	MiddleVertex middleCell;
	BiochamGraph graph;
	double X,Y;
	double iv_h, iv_w;
	Color color;
	int distance=20;
	
	public IntermediateVertex2(DefaultGraphCell middleCell) {
		
		
		if(middleCell!=null){
			
			if(middleCell instanceof IntermediateVertex){
				
				IntermediateVertex middle=(IntermediateVertex)middleCell;
				if(middle!=null){
				
					if(middle.getX()!=0 && middle.getY()!=0){
						initialize(middle.getX(),middle.getY());
					}				
				}
				graph=middle.getGraph();
				this.iv_h=IntermediateVertex.HEIGHT;
				this.iv_w=IntermediateVertex.WIDTH;
			}else if(middleCell instanceof IntermediateVertexAssoc){
				IntermediateVertexAssoc middle=(IntermediateVertexAssoc)middleCell;
				if(middle!=null){
				
					if(middle.getX()!=0 && middle.getY()!=0){
						initialize(middle.getX(),middle.getY());
					}				
				}
				graph=middle.getGraph();
				this.iv_h=IntermediateVertexAssoc.HEIGHT;
				this.iv_w=IntermediateVertexAssoc.WIDTH;
			}
		}
		 
	 }
		
	public IntermediateVertex2(BiochamGraph graph,double x, double y) {
		
		this.graph=graph;
		initialize(x,y);		
		 
	 }
	
	public void setBounds(double x, double y){
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();		
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(x,y,WIDTH,HEIGHT));
		setX(x);
		setY(y);
		
		nested.put(this, attributeMap1 );
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested);
		}
	}
	
	
	
	
	
	private void initialize(double x, double y) {
		
		BiochamGraphConstants.setBounds(this.getAttributes(), new Rectangle2D.Double(x-distance,y+5,WIDTH,HEIGHT));
		setX(x-distance);
		setY(y+5);
		BiochamGraphConstants.setEditable(this.getAttributes(), false);
		BiochamGraphConstants.setSelectable(this.getAttributes(),false);
		BiochamGraphConstants.setMoveable(this.getAttributes(),true);
		BiochamGraphConstants.setSizeable(this.getAttributes(), false);		
		BiochamGraphConstants.setResize(this.getAttributes(), false);			
		BiochamGraphConstants.setConstrained(this.getAttributes(),true);
		BiochamGraphConstants.setDisconnectable(this.getAttributes(), false);
	}
	 
	
	public double getX(){
		return X;	
	}
	public double getY(){
		return Y;
	}
	public void setX(double x) {
		X = x;
	}

	public void setY(double y) {
		Y = y;
	}
	
	public void setXYApply(double x,double y) {
		Utils.debugMsg("Vertex2 before:"+X+","+Y);
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();	
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(x,y,WIDTH,HEIGHT));
		setX(x);
		setY(y);
		nested.put(this, attributeMap1 );
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested);
		}
		Utils.debugMsg("Vertex2 after:"+X+","+Y);
	}

	
	
	public void setVerticalDown(double x, double y){
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();
		double v1=x+this.iv_w/2;
		double v2=y-distance;
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(v1,v2,WIDTH,HEIGHT));
		setX(v1);
		setY(v2);
		nested.put(this, attributeMap1 );
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested);
		}
		
	}
	
	public void setVerticalUp(double x, double y){
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();
		double v1=x+this.iv_w/2;
		double v2=y+distance+5;
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(v1,v2,WIDTH,HEIGHT));
		setX(v1);
		setY(v2);
		nested.put(this, attributeMap1 );
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested);
		}
		
	}
	
	public void setHorizontal(double x, double y){
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();
		double v1=x-this.iv_w-distance;
		double v2=y+this.iv_h/2;
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(v1,v2,WIDTH,HEIGHT));
		setX(v1);
		setY(v2);
		nested.put(this, attributeMap1 );
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested, null, null, null);
		}
		
	}

	public Color getColor() {
		return color;
	}

	public void setColor(Color color) {
		
		this.color = color;
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();
		BiochamGraphConstants.setLineColor(attributeMap1, color);
		nested.put(this, attributeMap1 );
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested, null, null, null);
		}
		if(this.userObject instanceof BiochamEdgeData){
			
			((BiochamEdgeData)this.userObject).setColor(color);
			BiochamGraphConstants.setBorderColor(this.getAttributes(), color);
		}
		
	}

	public int getDistance() {
		return distance;
	}

	public void setDistance(int distance) {
		this.distance = distance;
		BiochamGraphConstants.setBounds(this.getAttributes(), new Rectangle2D.Double(getX()-distance,getY(),WIDTH,HEIGHT));
		setX(getX()-distance);
		setY(getY());
	}
	
	
}
