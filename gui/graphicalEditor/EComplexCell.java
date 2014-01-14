package fr.inria.contraintes.biocham.graphicalEditor;


import org.jgraph.graph.DefaultGraphCell;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.modelData.ParamTableInitConc;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.geom.Rectangle2D;
import java.util.Hashtable;
import java.util.Map;
import java.util.UUID;



public class EComplexCell extends DefaultGraphCell {

	
	public static double WIDTH=EMacromoleculeCell.WIDTH*1.5;
	public static double HEIGHT=EMacromoleculeCell.HEIGHT*2;
	protected Rectangle2D bounds;
	private String macromoleculeName;
	String id;
	public double w, h;
	double x,y;
	
	public EComplexCell(Object userObject) {
		
		
			super(userObject);
			
			//if(userObject!=null && userObject instanceof BiochamEntityData){
				
				BiochamEntityData data=((BiochamEntityData)userObject);	
					
				
				if(data!=null){
					if(data.getContainingMolecules()!=null){
						HEIGHT=data.getContainingMolecules().size()*EMacromoleculeCell.HEIGHT+20;
					}else{
						HEIGHT=EMacromoleculeCell.HEIGHT+20;
					}
				}else{
					HEIGHT=EMacromoleculeCell.HEIGHT+20;
				}
				//HEIGHT+=20;
				//WIDTH+=20;
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
					data.setPosition(new Position(190,50,0,0));
				}
				initialize(x,y);
				
				UUID id = UUID.randomUUID();
				data.setId(id);
				this.id=id.toString();
				setUserObject(data);
				if(!BiochamDynamicTree.currentModel.isLoadingModel()){
					if(((ParamTableInitConc)data.getGraph().getBiochamModel().getInitConditions().getParamTable()).getOrigValues().indexOf(data.getName())>0){
						String s="present("+data.getName()+","+data.getInitialConcentration()+").\n";
						data.getGraph().getBiochamModel().sendToBiocham(s,"initConc");
						s=null;
					}	
				}		
				
			
				 if(((BiochamEntityData)userObject).getCompartment()!=null){
				    	if(!((BiochamEntityData)userObject).getCompartment().equals("Default") && !((BiochamEntityData)userObject).getCompartment().trim().equals("")){
				    		if(GraphUtilities.getCellByName(((BiochamEntityData)userObject).getGraph(),((BiochamEntityData)userObject).getCompartment())==null){
					    		BiochamCompartmentData data2=new BiochamCompartmentData(((BiochamEntityData)userObject).getGraph());
								data2.setCompartmentName(((BiochamEntityData)userObject).getCompartment());				
								ECompartmentCell compartment=new ECompartmentCell(data2);
								((BiochamEntityData)userObject).getGraph().getGraphLayoutCache().insert(compartment);
								((BiochamEntityData)userObject).getGraph().getGraphLayoutCache().toBack(new Object[]{compartment});
								((BiochamEntityData)userObject).getGraph().setMoveIntoGroups(false);
					    	}
				    	}
				    	
				 }
				 id=null;
					pos=null;
					size=null;
					data=null;
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
	   
	public void setPosition(BiochamGraph graph,double x, double y){
		Utils.debugMsg("Molecule "+getMacromoleculeName()+" has moved from ("+this.x+","+this.y+"), or "+"("+((BiochamEntityData)userObject).getPosition().getX()+","+((BiochamEntityData)userObject).getPosition().getY()+")"+" to ("+x+","+y+").");
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

	public String getMacromoleculeName() {
		if(macromoleculeName==null){
			this.macromoleculeName=((BiochamEntityData)userObject).getName();
		}
		return macromoleculeName;
	}
	public void setMacromoleculeName(String macromoleculeName) {
		this.macromoleculeName = macromoleculeName;
	}	 
	public String toString(){
		return "";//macromoleculeName;
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
		BiochamEntityData data=((BiochamEntityData)userObject);	
		Utils.debugMsg("positionAFTTER: "+data.getPosition().getX()+","+data.getPosition().getY());
		Utils.debugMsg("positionAFTTER: "+x+","+y);
	}	
}
