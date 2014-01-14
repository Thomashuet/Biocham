package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultGraphCell;

import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.geom.Rectangle2D;
import java.util.Hashtable;
import java.util.Map;
import java.util.UUID;

public class ESourceSink extends DefaultGraphCell {


	
	String id;
	/** Reference to the bounds attribute */
	protected Rectangle2D bounds;
	BiochamEntityData data;
	double x, y;
	
	public ESourceSink(Object userObject) {
		
		super(userObject);
		data=((BiochamEntityData)userObject);
		data.setId(UUID.randomUUID());
		
		this.id=data.getId().toString();
		Position pos=((BiochamEntityData)userObject).getPosition();
		bounds=BiochamGraphConstants.getBounds(this.getAttributes());
		if(pos!=null && bounds!=null){				
			BiochamGraphConstants.setBounds(this.getAttributes(),new Rectangle2D.Double(pos.getX(),pos.getY(),bounds.getWidth(),bounds.getHeight()));				
		}else{
			BiochamGraphConstants.setBounds(this.getAttributes(), new Rectangle2D.Double(40,40,25,25));
			pos=new Position(40,40,0,0);
			data.setPosition(pos);
		}
		if(pos!=null){
			BiochamGraphConstants.setBounds(this.getAttributes(),new Rectangle2D.Double(pos.getX(),pos.getY(),25,25));
		}
		bounds=BiochamGraphConstants.getBounds(this.getAttributes());
		BiochamGraphConstants.setEditable(this.getAttributes(), false);
		BiochamGraphConstants.setSelectable(this.getAttributes(),true);
		BiochamGraphConstants.setMoveable(this.getAttributes(),true);
		BiochamGraphConstants.setSizeable(this.getAttributes(), false);		
		BiochamGraphConstants.setResize(this.getAttributes(), false);			
		BiochamGraphConstants.setConstrained(this.getAttributes(),false);
		BiochamGraphConstants.setDisconnectable(this.getAttributes(), false);
		Utils.debugMsg("*********************************>>>>>>>>>>>>>>>>>>>>CREATED SS id="+this.id);
	}
	    
	//public ESourceSink() {}		
	public String toString(){ return "";}

	public String getId() {
		return id;
	}
	public void setPosition(BiochamGraph graph,double x, double y){
		Utils.debugMsg("Source/Sink "+this.id+" has moved from ("+((BiochamEntityData)userObject).getPosition().getX()+","+((BiochamEntityData)userObject).getPosition().getY()+")"+" to ("+x+","+y+").");
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(x,y,25,25));
		nested.put(this, attributeMap1);	
		setX(x);
		setY(y);
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested);
		}	
		graph.refresh();
	}
	public void setId(String id) {
		this.id = id;
	}

	public void setX(double xx){
		this.x=xx;
		data.getPosition().setX(xx);
	}
	public void setY(double yy){
		this.y=yy;
		data.getPosition().setY(yy);
	}
	
	public double getHeight(){
		return bounds.getHeight();
	}
	public double getWidth(){
		return bounds.getWidth();
	}
	public Rectangle2D getBounds() {
		return bounds;
	}

	public double getX() {
		return x;
	}

	public double getY() {
		return y;
	}
}
