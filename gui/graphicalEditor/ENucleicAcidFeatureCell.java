package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultGraphCell;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.modelData.ParamTableInitConc;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.geom.Rectangle2D;
import java.util.Hashtable;
import java.util.Map;
import java.util.UUID;



public class ENucleicAcidFeatureCell extends DefaultGraphCell {

	
	
	public static int WIDTH=70;
	public static int HEIGHT=30;
	public final static Rectangle2D defaultBounds = new Rectangle2D.Double(40,40,WIDTH,HEIGHT);
	protected Rectangle2D bounds;
	public double w, h,x,y;		
	private String macromoleculeName;
	String id;
	
	
	public ENucleicAcidFeatureCell(Object userObject) {
		
			super(userObject);
			String name=((BiochamEntityData)userObject).getName();			
			macromoleculeName=name;
			UUID id = UUID.randomUUID();
			((BiochamEntityData)userObject).setId(id);
			this.id=id.toString();
		
			MoleculeSize size=((BiochamEntityData)userObject).getSize();
			if(size!=null){
				if(size.getWidth()!=0 && size.getHeight()!=0){
					w=size.getWidth();
					h=(int)size.getHeight();
				}
			}else{
				((BiochamEntityData)userObject).setSize(new MoleculeSize(WIDTH,HEIGHT));
				w=WIDTH;
				h=HEIGHT;
			}
			
			Position pos=((BiochamEntityData)userObject).getPosition();
			if(pos!=null){
				x=pos.getX();
				y=pos.getY();	
			}else{
				x=190;
				y=50;
				((BiochamEntityData)userObject).setPosition(new Position(50,140,0,0));
			}
			initialize(x,y);		
			
			if(!BiochamDynamicTree.currentModel.isLoadingModel()){
				if(((ParamTableInitConc)((BiochamEntityData)userObject).getGraph().getBiochamModel().getInitConditions().getParamTable()).indexOf(name)>0){
					String s="present("+name+","+((BiochamEntityData)userObject).getInitialConcentration()+").\n";
					((BiochamEntityData)userObject).getGraph().getBiochamModel().sendToBiocham(s,"initConc");
					s=null;
				}
			}
			 if(((BiochamEntityData)userObject).getCompartment()!=null){
			    	if(!((BiochamEntityData)userObject).getCompartment().equals("Default") && !((BiochamEntityData)userObject).getCompartment().trim().equals("")){
			    		if(GraphUtilities.getCellByName(((BiochamEntityData)userObject).getGraph(),((BiochamEntityData)userObject).getCompartment())==null){
				    		BiochamCompartmentData data=new BiochamCompartmentData(((BiochamEntityData)userObject).getGraph());
							data.setCompartmentName(((BiochamEntityData)userObject).getCompartment());				
							ECompartmentCell compartment=new ECompartmentCell(data);
							((BiochamEntityData)userObject).getGraph().getGraphLayoutCache().insert(compartment);
							((BiochamEntityData)userObject).getGraph().getGraphLayoutCache().toBack(new Object[]{compartment});
							((BiochamEntityData)userObject).getGraph().setMoveIntoGroups(false);
				    	}
			    	}
			    	
			    }
			
	}
    
	private void initialize(double x, double y) {
		
		BiochamGraphConstants.setBounds(this.getAttributes(), new Rectangle2D.Double(x,y,w,h));
		BiochamGraphConstants.setEditable(this.getAttributes(), true);
		BiochamGraphConstants.setSelectable(this.getAttributes(),true);
		BiochamGraphConstants.setMoveable(this.getAttributes(),true);
		BiochamGraphConstants.setSizeable(this.getAttributes(), true);		
		BiochamGraphConstants.setResize(this.getAttributes(), true);			
		BiochamGraphConstants.setConstrained(this.getAttributes(),false);
		BiochamGraphConstants.setDisconnectable(this.getAttributes(), false);
	}
	
	
	public String getMacromoleculeName() {
		return macromoleculeName;
	}

	public void setMacromoleculeName(String macromoleculeName) {
		this.macromoleculeName = macromoleculeName;
	}
	public void setPosition(BiochamGraph graph,double x, double y){
		Utils.debugMsg("Molecule "+this.macromoleculeName+" has moved from ("+this.x+","+this.y+"), or "+"("+((BiochamEntityData)userObject).getPosition().getX()+","+((BiochamEntityData)userObject).getPosition().getY()+")"+" to ("+x+","+y+").");
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(x,y,w,h));
		nested.put(this, attributeMap1);		
		setX(x);
		setY(y);	
		((BiochamEntityData)userObject).setPosition(new Position(x,y,0,0));		
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested);
		}	
		graph.refresh();
	}
	public String toString(){
		return "";
	}
	
	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}
	
	public double getH() {
		return h;
	}

	public void setH(double h) {
		this.h = h;
	}

	public double getW() {
		return w;
	}

	public void setW(double w) {
		this.w = w;
	}

	public double getX() {
		return x;
	}

	public void setX(double x) {
		this.x = x;
	}

	public double getY() {
		return y;
	}

	public void setY(double y) {
		this.y = y;
	}

	
}
