package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultGraphCell;

import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.geom.Rectangle2D;
import java.util.Hashtable;
import java.util.Map;

public class IntermediateVertex3 extends DefaultGraphCell {

	
	
	
	public static int WIDTH=1;
	public static int HEIGHT=0;
	BiochamGraph graph;
	MiddleVertex middleCell;
	double X,Y;
	double iv_h, iv_w;
	Color color;
	int distance=20;
	
	public IntermediateVertex3(DefaultGraphCell middleCell) {
		
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
				
			}else if(middleCell instanceof IntermediateVertexDissoc){
				IntermediateVertexDissoc middle=(IntermediateVertexDissoc)middleCell;
				if(middle!=null){
				
					if(middle.getX()!=0 && middle.getY()!=0){
						initialize(middle.getX(),middle.getY());
					}				
				}
				graph=middle.getGraph();
				this.iv_h=IntermediateVertexDissoc.HEIGHT;
				this.iv_w=IntermediateVertexDissoc.WIDTH;
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

	public IntermediateVertex3(BiochamGraph graph,double x, double y) {
	
		this.graph=graph;
		initialize(x,y);	 
		 
	 }

	public void setOpositeDirection(){
		double xb=getX();
		double yb=getY();
		BiochamGraphConstants.setBounds(this.getAttributes(), new Rectangle2D.Double(getX()-2*distance,getY(),WIDTH,HEIGHT));
		setX(getX()-2*distance);
		setY(getY());
		double xa=getX();
		double ya=getY();
	}
	
	
	
	
	
	
	
	
	private void initialize(double x, double y) {
	
		BiochamGraphConstants.setBounds(this.getAttributes(), new Rectangle2D.Double(x+this.iv_w+distance,y+5,WIDTH,HEIGHT));
		setX(x+this.iv_w+distance);
		setY(y+5);
		Utils.debugMsg("\n\n******x="+getX()+", y="+getY());
		BiochamGraphConstants.setEditable(this.getAttributes(), false);
		BiochamGraphConstants.setSelectable(this.getAttributes(),false);
		BiochamGraphConstants.setMoveable(this.getAttributes(),true);
		BiochamGraphConstants.setSizeable(this.getAttributes(), false);		
		BiochamGraphConstants.setResize(this.getAttributes(), false);			
		BiochamGraphConstants.setConstrained(this.getAttributes(),true);
		BiochamGraphConstants.setDisconnectable(this.getAttributes(), false);
		
	}
	public void setXYApply(double x,double y) {
		Utils.debugMsg("Vertex3 before:"+X+","+Y);
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();	
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(x,y,WIDTH,HEIGHT));
		setX(x);
		setY(y);
		nested.put(this, attributeMap1 );
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested);
		}
		Utils.debugMsg("Vertex3 after:"+X+","+Y);
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
	public void setVerticalUp(double x, double y){
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();
		double v1=x+this.iv_w/2;
		double v2=y+this.iv_h+distance;
		
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(v1,v2,WIDTH,HEIGHT));
		setX(v1);
		setY(v2);
		
		nested.put(this, attributeMap1 );
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested);
		}	
	}
	
	public void setVerticalDown(double x, double y){
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();
		double v1=x+this.iv_w/2;
		double v2=y-this.iv_h-distance;
		
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
		double v1=x+this.iv_w+distance;
		double v2=y+this.iv_h/2;
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(v1,v2,WIDTH,HEIGHT));
		setX(v1);
		setY(v2);
		nested.put(this, attributeMap1 );
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested, null, null, null);
		}		
	}

	public double getX() {
		return X;
	}

	public void setX(double x) {
		X = x;
	}

	public double getY() {
		return Y;
	}

	public void setY(double y) {
		Y = y;
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
		BiochamGraphConstants.setBounds(this.getAttributes(), new Rectangle2D.Double(getX()+this.iv_w+distance,getY(),WIDTH,HEIGHT));
		setX(getX()+this.iv_w+distance);
		setY(getY());
	}
	
	
	
}