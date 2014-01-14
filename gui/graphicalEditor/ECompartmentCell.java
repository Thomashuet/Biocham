package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultGraphCell;
import java.awt.geom.Rectangle2D;



public class ECompartmentCell extends DefaultGraphCell {

	
	
	
	public static int WIDTH=400;
	public static int HEIGHT=300;
	public final static Rectangle2D defaultBounds = new Rectangle2D.Double(0,0,WIDTH,HEIGHT);
	protected Rectangle2D bounds;
	private String name;	
	BiochamCompartmentData data;
	public static int callings2=0; 
	
	public ECompartmentCell(BiochamCompartmentData userObject) {
		
		super(userObject);
		if(userObject!=null && userObject instanceof BiochamCompartmentData){
			name=((BiochamCompartmentData)userObject).getCompartmentName();
		}		
		Position pos=((BiochamCompartmentData)userObject).getPosition();
		if(pos!=null){
			 BiochamGraphConstants.setBounds(this.getAttributes(), new Rectangle2D.Double(pos.getX(),pos.getY(),WIDTH,HEIGHT));
		}else{
			 BiochamGraphConstants.setBounds(this.getAttributes(), new Rectangle2D.Float(0,0,WIDTH,HEIGHT));
		}
		initialize();
		userObject.getGraph().setNumberOfCompartments(userObject.getGraph().getNumberOfCompartments()+1);
		//System.out.println("\n**********CALLING COMPART_CELL: "+callings2);
		callings2++;
		
	}

	
	private void initialize() {
		
		 BiochamGraphConstants.setEditable(this.getAttributes(), true);
		 BiochamGraphConstants.setSelectable(this.getAttributes(),true);
		 BiochamGraphConstants.setMoveable(this.getAttributes(),true);
		 BiochamGraphConstants.setSizeable(this.getAttributes(), true);		
		 BiochamGraphConstants.setResize(this.getAttributes(), true);			
		 BiochamGraphConstants.setConstrained(this.getAttributes(),false);
		 BiochamGraphConstants.setDisconnectable(this.getAttributes(), false);
		 BiochamGraphConstants.setInset(this.getAttributes(), 50);
	}
	    
	
	
	public String toString(){
		return "";
	}


	public BiochamCompartmentData getData() {
		return data;
	}


	public void setData(BiochamCompartmentData data) {
		this.data = data;
	}


	public String getName() {
		return name;
	}


	public void setName(String name) {
		this.name = name;
	}

}
