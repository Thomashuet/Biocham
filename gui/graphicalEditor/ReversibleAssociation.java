package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.modelData.ParamTableRules;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Map;
import java.util.UUID;
import javax.swing.JLabel;
import javax.swing.JOptionPane;




public class ReversibleAssociation extends DefaultGraphCell {

	
	private DefaultGraphCell[] children;
	private BiochamGraph graph;
	IntermediateVertexAssoc intermediateVertexMIDDLE1;
	IntermediateVertexDissoc intermediateVertexMIDDLE2;
	IntermediateVertex2 intermediateVertexLEFT1,intermediateVertexLEFT2;
	IntermediateVertex3 intermediateVertexRIGHT1,intermediateVertexRIGHT2;	
	ArrayList<BiochamObject> sources;
	ArrayList<UUID> containingMolecules;
	ArrayList<DefaultGraphCell> modulators1,modulators2;
	BiochamEdgeData userObject;	
	BiochamObject target;	
	String id;	
	String leftSide,rightSide;
	Color color;
	Location nextSourceLocation,nextTargetLocation, nextModulatorLocation1,nextModulatorLocation2;
	DefaultGraphCell middleEdge1,middleEdge2;
	boolean reversibleDissociation=false;
	
	public ReversibleAssociation(Object cell,ArrayList<BiochamObject> sources,BiochamObject target,DefaultGraphCell modulator,BiochamGraph graphic,String rule){
		super(cell);
		createAssociation(cell, sources, target, modulator, graphic, rule);	
	}
	public ReversibleAssociation(Object cell, ArrayList<DefaultGraphCell> reacts, DefaultGraphCell prods, DefaultGraphCell modulator,BiochamGraph graph, String rule) {
		super(cell);		
		ArrayList<BiochamObject> sources=new ArrayList<BiochamObject>();
		BiochamObject bo=null;
		for(int i=0;i<reacts.size();i++){
			((BiochamEntityData)reacts.get(i).getUserObject()).setReactant(true);
			bo=new BiochamObject(reacts.get(i).addPort(),((BiochamEntityData)reacts.get(i).getUserObject()).getMultimerCardinality(),((BiochamEntityData)reacts.get(i).getUserObject()).getName(),reacts.get(i));
			sources.add(bo);
			bo=null;
		}
		((BiochamEntityData)prods.getUserObject()).setReactant(true);
		createAssociation(cell,sources,new BiochamObject(prods.addPort(),((BiochamEntityData)prods.getUserObject()).getMultimerCardinality(),((BiochamEntityData)prods.getUserObject()).getName(),prods), modulator, graph, rule);
		sources=null;
	}
	private void createAssociation(Object cell, ArrayList<BiochamObject> sources, BiochamObject target, DefaultGraphCell modulator, BiochamGraph graphic, String rule) {
		

		
		
		graph=graphic;		
		containingMolecules=new ArrayList<UUID>();
		userObject=(BiochamEdgeData) cell;
		userObject.setStoichiometry(target.getStoichiometry());
		ArrayList comps=new ArrayList();
		id=rule;
		userObject.setReversibilityType(ReversibilityType.DOUBLE);			
		this.sources=new ArrayList<BiochamObject>(sources);
		this.target=new BiochamObject(target.getInstance().addPort(),target.getStoichiometry(),((BiochamEntityData)target.getInstance().getUserObject()).getName(),target.getInstance());		
		int size=2+2*sources.size()+2;//2xMiddleEdge, 2*sourcesSizeEdges,2xNormalEdgesToTarget
		children=new DefaultGraphCell[size];		
		
		DefaultGraphCell mEdge1,mEdge2;
		BiochamEdgeData dt1=new BiochamEdgeData(userObject);
		mEdge1=createMiddleEdge(dt1,graphic);		
		this.intermediateVertexLEFT1=((MiddleEdgeAssociation)mEdge1).getLEFTIntermediateVertex();
		this.intermediateVertexMIDDLE1=((MiddleEdgeAssociation)mEdge1).getMIDDLEIntermediateVertex();
		BiochamEdgeData dt2=new BiochamEdgeData(userObject);
		mEdge2=createMiddleEdge(dt2,graphic,this.intermediateVertexMIDDLE1.getX(),this.intermediateVertexMIDDLE1.getY()-25);	
		this.intermediateVertexRIGHT2=((MiddleEdgeDissociation)mEdge2).getRIGHTIntermediateVertex();
		this.intermediateVertexMIDDLE2=((MiddleEdgeDissociation)mEdge2).getMIDDLEIntermediateVertex();
	
		children[0]=mEdge1;	
		setMiddleEdge1(mEdge1);
		children[1]=mEdge2;	
		setMiddleEdge2(mEdge2);
		comps.add(mEdge1);
		comps.add(mEdge2);
		
		// n -times.....
		for(int i=0;i<sources.size();i++){
			
			DefaultEdge edge1, edge2;
			ArrayList<UUID> molecules=new ArrayList<UUID>();
			BiochamEdgeData eData=new BiochamEdgeData(graph,molecules);
			int stoich1=sources.get(i).getStoichiometry();
			int stoich=((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getStoichoimetryIntegerForReaction();
			if(stoich1>1 || stoich>1){				
				if(stoich>1){
					eData.setStoichiometry(stoich);
				}else{
					eData.setStoichiometry(sources.get(i).getStoichiometry());
				}
								
				edge1 = new BiochamEdge(eData);		
				BiochamGraphConstants.setSelectable(edge1.getAttributes(),false);
				BiochamGraphConstants.setLabelAlongEdge(edge1.getAttributes(), true);
				BiochamGraphConstants.setLabelEnabled(edge1.getAttributes(),true);
				BiochamGraphConstants.setExtraLabels(edge1.getAttributes(),new Object[]{new JLabel(Integer.toString(eData.getStoichiometry()))});
				molecules.add(((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getId());
				eData.setMolecules(molecules);
				edge1.setUserObject(eData);
				edge2 = new BiochamEdge(eData);		
				BiochamGraphConstants.setSelectable(edge2.getAttributes(),false);
				BiochamGraphConstants.setLabelAlongEdge(edge2.getAttributes(), true);
				BiochamGraphConstants.setLabelEnabled(edge2.getAttributes(),true);
				BiochamGraphConstants.setExtraLabels(edge2.getAttributes(),new Object[]{new JLabel(Integer.toString(eData.getStoichiometry()))});				
				edge2.setUserObject(eData);
				BiochamGraphConstants.setLineEnd(edge2.getAttributes(), BiochamGraphConstants.ARROW_CLASSIC);
				BiochamGraphConstants.setEndFill(edge2.getAttributes(), true);
				
			}else{
				 edge1 = new DefaultEdge();
				 BiochamGraphConstants.setSelectable(edge1.getAttributes(),true);
				 BiochamGraphConstants.setLineStyle(edge1.getAttributes(), BiochamGraphConstants.STYLE_ORTHOGONAL);
				 BiochamGraphConstants.setBendable(edge1.getAttributes(), false);
				 edge2 = new DefaultEdge();
				 BiochamGraphConstants.setSelectable(edge2.getAttributes(),true);
				 BiochamGraphConstants.setLineStyle(edge2.getAttributes(), BiochamGraphConstants.STYLE_ORTHOGONAL);
				 BiochamGraphConstants.setBendable(edge2.getAttributes(), false);
				 BiochamGraphConstants.setLineEnd(edge2.getAttributes(), BiochamGraphConstants.ARROW_CLASSIC);
				 BiochamGraphConstants.setEndFill(edge2.getAttributes(), true);
				 
			}
			int k=((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getNumberOfConnections();
			k+=1;
			((BiochamEntityData)sources.get(i).getInstance().getUserObject()).setNumberOfConnections(k);
			((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getInvolvedInReactions().add(rule);
			if(k>1){
				((BiochamEntityData)sources.get(i).getInstance().getUserObject()).setResponsable(true);
			}
			edge1.setSource(sources.get(i).getPort());			
			edge1.setTarget(((MiddleEdgeAssociation)mEdge1).getLEFTIntermediateVertex().addPort());	
			edge2.setTarget(sources.get(i).getInstance().addPort());			
			edge2.setSource(((MiddleEdgeDissociation)mEdge2).getRIGHTIntermediateVertex().addPort());		
			children[2*i+2] = edge1;	
			children[2*i+2+1] = edge2;
			comps.add(edge1);
			comps.add(edge2);
			comps.add(sources.get(i).getInstance());			
			molecules=null;
			eData=null;
			edge1=null;
			edge2=null;
			
		}
		
				
		DefaultEdge edge1 = new DefaultEdge();		
		BiochamGraphConstants.setSelectable(edge1.getAttributes(),true);
		BiochamGraphConstants.setLineStyle(edge1.getAttributes(), BiochamGraphConstants.STYLE_ORTHOGONAL);
		BiochamGraphConstants.setBendable(edge1.getAttributes(), false);
		edge1.setSource(((MiddleEdgeAssociation)mEdge1).getMIDDLEIntermediateVertex().addPort());
		edge1.setTarget(target.getPort());
		BiochamGraphConstants.setLineEnd(edge1.getAttributes(), BiochamGraphConstants.ARROW_CLASSIC);
		BiochamGraphConstants.setEndFill(edge1.getAttributes(), true);
		
		DefaultEdge edge2 = new DefaultEdge();		
		BiochamGraphConstants.setSelectable(edge2.getAttributes(),true);
		BiochamGraphConstants.setLineStyle(edge2.getAttributes(), BiochamGraphConstants.STYLE_ORTHOGONAL);
		BiochamGraphConstants.setBendable(edge2.getAttributes(), false);
		edge2.setTarget(((MiddleEdgeDissociation)mEdge2).getMIDDLEIntermediateVertex().addPort());
		edge2.setSource(target.getInstance().addPort());
		
		int k=((BiochamEntityData)target.getInstance().getUserObject()).getNumberOfConnections();
		k+=1;
		BiochamGraphConstants.setSelectable(edge1.getAttributes(),false);
		((BiochamEntityData)target.getInstance().getUserObject()).setNumberOfConnections(k);
		((BiochamEntityData)target.getInstance().getUserObject()).getInvolvedInReactions().add(rule);
		if(k>1){
			((BiochamEntityData)target.getInstance().getUserObject()).setResponsable(true);
		}			
		comps.add(target.getInstance());		
		children[sources.size()*2+2] = edge1;
		children[sources.size()*2+3] = edge2;
		
		comps.add(edge1);
		comps.add(edge2);
		
		ArrayList<BiochamObject> ar=new ArrayList<BiochamObject>(1);
		ar.add(target);
		/*if(cX<100){
			cX=140;
			this.intermediateVertexMIDDLE.setBounds(cX,cY);		
		}
		if(!opposite){
			if(sources.size()==1){
				GraphUtilities.setSourcesLocation(graph,sources,cX-30,cY+6);
				GraphUtilities.setTargetsLocation(graph,ar,cX,cY+5);
			}else{
				GraphUtilities.setSourcesLocation(graph,sources,cX-10,cY+10);
				GraphUtilities.setTargetsLocation(graph,ar,cX,cY);
			}
		}*/
		
		double cX=this.intermediateVertexMIDDLE1.getX();
		double cY=this.intermediateVertexMIDDLE1.getY();	
		/*if(cX<300){
			cX=cX*2;
			this.intermediateVertexMIDDLE1.setBounds(cX,cY);		
		}*/
		if(sources.size()==1){
			GraphUtilities.setSourcesLocation(graph,sources,cX-30,cY+6);
			GraphUtilities.setTargetsLocation(graph,ar,cX,cY+5);
		}else{
			GraphUtilities.setSourcesLocation(graph,sources,cX-50,cY+10);
			GraphUtilities.setTargetsLocation(graph,ar,cX,cY);
		}
		
		
		
		
		
		graphic.getGraphLayoutCache().insertGroup(this,children);
		
		
		
		
		BiochamGraphConstants.setRouting(this.getAttributes(), MyParallelEdgeRouter.getSharedInstance());
		BiochamGraphConstants.setEditable(this.getAttributes(),true);
		BiochamGraphConstants.setDisconnectable(this.getAttributes(),false);
		BiochamGraphConstants.setGroupOpaque(this.getAttributes(),true);
		BiochamGraphConstants.setMoveable(this.getAttributes(),true);
		BiochamGraphConstants.setSelectable(this.getAttributes(),true);		
		
		graph=graphic;
		graphic.getContainingRules().put(rule.trim(),comps.toArray());
			
		GraphUtilities.setLineStyleOrthogonalSelectedCells(graph);		
		userObject.setName(id);
		this.setUserObject(userObject);
		cell=userObject;
		super.setUserObject(userObject);	
		containingMolecules=userObject.getMolecules();
		graphic.updateReaction(rule,null);
		ar=null;
		comps.clear();
		comps=null;		
		GraphUtilities.setLineStyleOrthogonalSelectedCells(graph);
	
		
	}

	public IntermediateVertex2 getIntermediateVertexLEFT1() {
		return intermediateVertexLEFT1;
	}

	public void setIntermediateVertexLEFT1(
			IntermediateVertex2 intermediateVertexLEFT1) {
		this.intermediateVertexLEFT1 = intermediateVertexLEFT1;
	}

	public IntermediateVertex2 getIntermediateVertexLEFT2() {
		return intermediateVertexLEFT2;
	}

	public void setIntermediateVertexLEFT2(
			IntermediateVertex2 intermediateVertexLEFT2) {
		this.intermediateVertexLEFT2 = intermediateVertexLEFT2;
	}

	public IntermediateVertexAssoc getIntermediateVertexMIDDLE1() {
		return intermediateVertexMIDDLE1;
	}

	public void setIntermediateVertexMIDDLE1(
			IntermediateVertexAssoc intermediateVertexMIDDLE1) {
		this.intermediateVertexMIDDLE1 = intermediateVertexMIDDLE1;
	}

	public IntermediateVertexDissoc getIntermediateVertexMIDDLE2() {
		return intermediateVertexMIDDLE2;
	}

	public void setIntermediateVertexMIDDLE2(
			IntermediateVertexDissoc intermediateVertexMIDDLE2) {
		this.intermediateVertexMIDDLE2 = intermediateVertexMIDDLE2;
	}

	public IntermediateVertex3 getIntermediateVertexRIGHT1() {
		return intermediateVertexRIGHT1;
	}

	public void setIntermediateVertexRIGHT1(
			IntermediateVertex3 intermediateVertexRIGHT1) {
		this.intermediateVertexRIGHT1 = intermediateVertexRIGHT1;
	}

	public IntermediateVertex3 getIntermediateVertexRIGHT2() {
		return intermediateVertexRIGHT2;
	}

	public void setIntermediateVertexRIGHT2(
			IntermediateVertex3 intermediateVertexRIGHT2) {
		this.intermediateVertexRIGHT2 = intermediateVertexRIGHT2;
	}

	public DefaultGraphCell getMiddleEdge1() {
		return middleEdge1;
	}

	public void setMiddleEdge1(DefaultGraphCell middleEdge1) {
		this.middleEdge1 = middleEdge1;
	}

	public DefaultGraphCell getMiddleEdge2() {
		return middleEdge2;
	}

	public void setMiddleEdge2(DefaultGraphCell middleEdge2) {
		this.middleEdge2 = middleEdge2;
	}
	private DefaultGraphCell createMiddleEdge(Object cell,BiochamGraph graphic, double x, double y) {
				
		return new MiddleEdgeDissociation(cell,graphic,x,y);
	}
	private DefaultGraphCell createMiddleEdge(Object cell,BiochamGraph graphic) {
		
		double x,y;
		x=0;
		y=0;
		if(cell instanceof BiochamEdgeData){
			BiochamEdgeData d=(BiochamEdgeData)cell;
			Position pos=d.getPosition();
			
			if(pos!=null){
				try{
					x=pos.getX();
					y=pos.getY();
				}catch(Exception e){}
			}
		}		
		return new MiddleEdgeAssociation(cell,graphic,x,y,false);
	}
	
	public void setVerticalUp(){	
		this.getIntermediateVertexMIDDLE1().setBounds(this.getIntermediateVertexMIDDLE2().getX()-30,this.getIntermediateVertexMIDDLE2().getY());
		this.getIntermediateVertexLEFT1().setBounds(intermediateVertexMIDDLE1.getX()+intermediateVertexMIDDLE1.WIDTH/2,intermediateVertexMIDDLE1.getY()-15);
		this.getIntermediateVertexRIGHT2().setBounds(intermediateVertexMIDDLE2.getX()+intermediateVertexMIDDLE2.WIDTH/2,intermediateVertexMIDDLE2.getY()-15);
	}
	public void setVerticalDown(){	
		this.getIntermediateVertexMIDDLE1().setBounds(this.getIntermediateVertexMIDDLE2().getX()+30,this.getIntermediateVertexMIDDLE2().getY());
		this.getIntermediateVertexLEFT1().setBounds(intermediateVertexMIDDLE1.getX()+intermediateVertexMIDDLE1.WIDTH/2,intermediateVertexMIDDLE1.getY()+20);
		this.getIntermediateVertexRIGHT2().setBounds(intermediateVertexMIDDLE2.getX()+intermediateVertexMIDDLE2.WIDTH/2,intermediateVertexMIDDLE2.getY()+20);
	}
	
	public void setHorizontal(){
		this.getIntermediateVertexMIDDLE1().setBounds(this.getIntermediateVertexMIDDLE2().getX(),this.getIntermediateVertexMIDDLE2().getY()+30);
		this.getIntermediateVertexLEFT1().setBounds(intermediateVertexMIDDLE1.getX()-15,intermediateVertexMIDDLE1.getY()+intermediateVertexMIDDLE1.WIDTH/2);
		this.getIntermediateVertexRIGHT2().setBounds(intermediateVertexMIDDLE2.getX()-15,intermediateVertexMIDDLE2.getY()+intermediateVertexMIDDLE2.WIDTH/2);		
	}

	public void setHorizontalReverse() {
		
		setHorizontal();
		
		((MiddleEdgeAssociation)this.getMiddleEdge1()).setHorizontalReverse();
		((MiddleEdgeDissociation)this.getMiddleEdge2()).setHorizontal();
		
		//this.getIntermediateVertexMIDDLE1().setBounds(this.getIntermediateVertexMIDDLE2().getX(),this.getIntermediateVertexMIDDLE2().getY()-5);
		
		/*this.getIntermediateVertexMIDDLE1().setBounds(this.getIntermediateVertexMIDDLE2().getX(),this.getIntermediateVertexMIDDLE2().getY()+30);
		this.getIntermediateVertexLEFT1().setBounds(intermediateVertexMIDDLE1.getX()-15,intermediateVertexMIDDLE1.getY()+intermediateVertexMIDDLE1.WIDTH/2);
		this.getIntermediateVertexRIGHT2().setBounds(intermediateVertexMIDDLE2.getX()-15,intermediateVertexMIDDLE2.getY()+intermediateVertexMIDDLE2.WIDTH/2);*/
		
		/*
		 * 
		 * ((MiddleEdge)this.getMiddleEdge1()).setHorizontalReverse();
		((MiddleEdge)this.getMiddleEdge2()).setHorizontalReverse();
		this.getIntermediateVertexMIDDLE2().setBounds(this.getIntermediateVertexMIDDLE2().getX(),this.getIntermediateVertexMIDDLE2().getY()-5);
		 * */
	}
	
	public void addReactant() {

		
		String rule=this.getId();	
		AddReactantProductDialog d=new AddReactantProductDialog("Choose Reactant", graph);
		Object[] newObjects=new Object[2];
		
		if(d.getChosenReactant()!=null && d.getChosenReactant()!=""){
					
			DefaultGraphCell cell=GraphUtilities.getCellByName(graph,d.getChosenReactant().trim());
			int stoich=d.getStoichCoeff();			
			DefaultEdge edge1;
			ArrayList<UUID> molecules=new ArrayList<UUID>();
			BiochamEdgeData eData=new BiochamEdgeData(graph,molecules);
			if(stoich>1){				
				
				eData.setStoichiometry(stoich);					
				edge1 = new BiochamEdge(eData);				
				BiochamGraphConstants.setLabelEnabled(edge1.getAttributes(),true);
				BiochamGraphConstants.setLabelAlongEdge(edge1.getAttributes(), true);
				BiochamGraphConstants.setExtraLabels(edge1.getAttributes(),new Object[]{new JLabel(Integer.toString(eData.getStoichiometry()))});				
				molecules.add(((BiochamEntityData)cell.getUserObject()).getId());
				eData.setMolecules(molecules);
				edge1.setUserObject(eData);
				
			}else{
				edge1 = new DefaultEdge();
				BiochamGraphConstants.setSelectable(edge1.getAttributes(),true);
				BiochamGraphConstants.setLineStyle(edge1.getAttributes(), BiochamGraphConstants.STYLE_ORTHOGONAL);
				BiochamGraphConstants.setBendable(edge1.getAttributes(), false);			
			}
			if(getColor()!=null){
				BiochamGraphConstants.setLineColor(edge1.getAttributes(), getColor());
			}
						
			BiochamObject o=new BiochamObject(cell.addPort(),stoich,((BiochamEntityData)cell.getUserObject()).getName(),cell);	
		
			userObject.getMolecules().add(((BiochamEntityData)cell.getUserObject()).getId());
			userObject.getReactants().add(new BiochamEnitity(((BiochamEntityData)cell.getUserObject()).getId(),((BiochamEntityData)cell.getUserObject()).getName()));
			this.setUserObject(userObject);
			super.setUserObject(userObject);
			containingMolecules=userObject.getMolecules();
			
			int p=((BiochamEntityData)o.getInstance().getUserObject()).getNumberOfConnections();
			p+=1;
			((BiochamEntityData)o.getInstance().getUserObject()).setNumberOfConnections(p);
			((BiochamEntityData)o.getInstance().getUserObject()).getInvolvedInReactions().add(rule);
			if(p>1){
				((BiochamEntityData)o.getInstance().getUserObject()).setResponsable(true);
			}
			
			Map nested = new Hashtable();
			Map attributeMap1 = new Hashtable();
			Location l=getNextSourceLocation();
			double x=l.getX();
			double y=l.getY()+GraphUtilities.getCellHeight(o.getInstance());				
			BiochamGraphConstants.setBounds(attributeMap1,new Rectangle2D.Double(x,y,GraphUtilities.getCellWidth(o.getInstance()),GraphUtilities.getCellHeight(o.getInstance())));			
			nested.put(o.getInstance(), attributeMap1 );
			if(graph!=null){
				graph.getGraphLayoutCache().edit(nested, null, null, null);
			}
			((BiochamEntityData)cell.getUserObject()).setPosition(new Position(x,y,0,0));
			l=null;
			
			edge1.setSource(o.getPort());
			this.getIntermediateVertexLEFT1().addPort();
			int len=getIntermediateVertexLEFT1().getChildCount();
			edge1.setTarget(getIntermediateVertexLEFT1().getChildAt(len-1));
			
			graph.getGraphLayoutCache().toFront(new Object[]{edge1,cell});			
			graph.getGraphLayoutCache().insert(edge1);
			graph.refresh();
			newObjects[0]=cell;
			newObjects[1]=edge1;
			sources.add(o);
			
			String newRule;
			String ruleToChange;
			String tmp;
			/*if(reversibleDissociation){
				ruleToChange=rule.substring(1,rule.indexOf(","));
				tmp=GraphUtilities.addReactantToBiochamRule(ruleToChange,((BiochamEntityData)cell.getUserObject()).getName());
				newRule="{"+tmp+rule.substring(rule.indexOf(","));
				ruleToChange=rule.substring(rule.indexOf(",")+1,rule.length()-1);
				tmp=GraphUtilities.addProductToBiochamRule(ruleToChange,((BiochamEntityData)cell.getUserObject()).getName());
				newRule=rule.substring(0,rule.indexOf(",")+1)+tmp+"}";
			}else{
				ruleToChange=rule.substring(1,rule.indexOf(","));
				tmp=GraphUtilities.addProductToBiochamRule(ruleToChange,((BiochamEntityData)cell.getUserObject()).getName());
				newRule="{"+tmp+rule.substring(rule.indexOf(","));
				ruleToChange=rule.substring(rule.indexOf(",")+1,rule.length()-1);
				tmp=GraphUtilities.addReactantToBiochamRule(ruleToChange,((BiochamEntityData)cell.getUserObject()).getName());
				newRule=rule.substring(0,rule.indexOf(",")+1)+tmp+"}";
				
			}*/
			if(reversibleDissociation){
				ruleToChange=rule.substring(rule.indexOf(",")+1,rule.length()-1);
				tmp=GraphUtilities.addReactantToBiochamRule(ruleToChange,((BiochamEntityData)cell.getUserObject()).getName());
				newRule=rule.substring(0,rule.indexOf(",")+1)+tmp+"}";
			}else{
				ruleToChange=rule.substring(1,rule.indexOf(","));
				tmp=GraphUtilities.addReactantToBiochamRule(ruleToChange,((BiochamEntityData)cell.getUserObject()).getName());
				newRule="{"+tmp+rule.substring(rule.indexOf(","));
			}		
						
			
			//graph.updateReaction(rule, newRule);
			((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).setFromGraph(true);
			graph.setAllAdded(true);
			((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).changeRules(ruleToChange,tmp);
			graph.setAllAdded(true);
			((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).setFromGraph(true);			
			userObject.setName(newRule);			
			this.setId(newRule);
			this.setUserObject(userObject);		
			GraphUtilities.modifyRuleFromContainingRules(graph, rule, newRule, newObjects);	
		}
		d.dispose();
		d=null;
		
	}

	public void addProduct() {

		
		
		String rule=this.getId();					
		AddReactantProductDialog d=new AddReactantProductDialog("Choose Product", graph);			
		Object[] newObjects=new Object[2];
		
		if(d.getChosenReactant()!=null && d.getChosenReactant()!=""){
										
			DefaultGraphCell cell=GraphUtilities.getCellByName(graph,d.getChosenReactant());			
			int stoich=d.getStoichCoeff();
			DefaultEdge edge;
			ArrayList<UUID> molecules=new ArrayList<UUID>();
			BiochamEdgeData eData=new BiochamEdgeData(graph,molecules);
			if(stoich>1){
					
					eData.setStoichiometry(stoich);					
					edge = new BiochamEdge(eData);									
					BiochamGraphConstants.setLabelEnabled(edge.getAttributes(),true);
					BiochamGraphConstants.setLabelAlongEdge(edge.getAttributes(), true);
					BiochamGraphConstants.setExtraLabels(edge.getAttributes(),new Object[]{new JLabel(Integer.toString(eData.getStoichiometry()))});
					molecules.add(((BiochamEntityData)cell.getUserObject()).getId());
					eData.setMolecules(molecules);
					edge.setUserObject(eData);
			}else{
					 edge = new DefaultEdge();
					 BiochamGraphConstants.setSelectable(edge.getAttributes(),true);
					 BiochamGraphConstants.setLineStyle(edge.getAttributes(), BiochamGraphConstants.STYLE_ORTHOGONAL);
					 BiochamGraphConstants.setBendable(edge.getAttributes(), false);
			}	
			
			if(getColor()!=null){
				BiochamGraphConstants.setLineColor(edge.getAttributes(), getColor());
			}
			
			
			BiochamObject o=new BiochamObject(cell.addPort(),stoich,((BiochamEntityData)cell.getUserObject()).getName(),cell);
		
			userObject.getMolecules().add(((BiochamEntityData)cell.getUserObject()).getId());
			userObject.getProducts().add(new BiochamEnitity(((BiochamEntityData)cell.getUserObject()).getId(),((BiochamEntityData)cell.getUserObject()).getName()));
			this.setUserObject(userObject);
			super.setUserObject(userObject);
			containingMolecules=userObject.getMolecules();	
			
										
			Map nested = new Hashtable();
			Map attributeMap1 = new Hashtable();
			Location l=getNextTargetLocation();
			double x=l.getX();
			double y=l.getY()+GraphUtilities.getCellHeight(o.getInstance());					
			BiochamGraphConstants.setBounds(attributeMap1,new Rectangle2D.Double(x,y,GraphUtilities.getCellWidth(o.getInstance()),GraphUtilities.getCellHeight(o.getInstance())));			
			nested.put(o.getInstance(), attributeMap1 );
			if(graph!=null){
				graph.getGraphLayoutCache().edit(nested, null, null, null);
			}
			((BiochamEntityData)cell.getUserObject()).setPosition(new Position(x,y,0,0));
			l=null;
			
			this.getIntermediateVertexRIGHT2().addPort();
			int len=getIntermediateVertexRIGHT2().getChildCount();
			edge.setSource(getIntermediateVertexRIGHT2().getChildAt(len-1));
			edge.setTarget(o.getPort());
			
			int p=((BiochamEntityData)o.getInstance().getUserObject()).getNumberOfConnections();
			p+=1;
			((BiochamEntityData)o.getInstance().getUserObject()).setNumberOfConnections(p);
			((BiochamEntityData)o.getInstance().getUserObject()).getInvolvedInReactions().add(rule);
			if(p>1){
				((BiochamEntityData)o.getInstance().getUserObject()).setResponsable(true);
			}
			
			BiochamGraphConstants.setLineEnd(edge.getAttributes(), BiochamGraphConstants.ARROW_CLASSIC);
			BiochamGraphConstants.setEndFill(edge.getAttributes(), true);	
			
			graph.getGraphLayoutCache().toFront(new Object[]{edge,cell});			
			graph.getGraphLayoutCache().insert(edge);
			graph.refresh();
			sources.add(o);
			newObjects[0]=cell; 
			newObjects[1]=edge;
			
			String newRule;
			String ruleToChange;
			String tmp;
			if(reversibleDissociation){
				ruleToChange=rule.substring(1,rule.indexOf(","));
				tmp=GraphUtilities.addProductToBiochamRule(ruleToChange,((BiochamEntityData)cell.getUserObject()).getName());
				newRule="{"+tmp+rule.substring(rule.indexOf(","));
				/*ruleToChange=rule.substring(rule.indexOf(",")+1,rule.length()-1);
				tmp=GraphUtilities.addProductToBiochamRule(ruleToChange,((BiochamEntityData)cell.getUserObject()).getName());
				newRule=rule.substring(0,rule.indexOf(",")+1)+tmp+"}";*/
			}else{
			/*	ruleToChange=rule.substring(1,rule.indexOf(","));
				tmp=GraphUtilities.addProductToBiochamRule(ruleToChange,((BiochamEntityData)cell.getUserObject()).getName());
				newRule="{"+tmp+rule.substring(rule.indexOf(","));*/
				ruleToChange=rule.substring(rule.indexOf(",")+1,rule.length()-1);
				tmp=GraphUtilities.addProductToBiochamRule(ruleToChange,((BiochamEntityData)cell.getUserObject()).getName());
				newRule=rule.substring(0,rule.indexOf(",")+1)+tmp+"}";
				
			}
			
							
					
			((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).setFromGraph(true);
			graph.setAllAdded(true);
			((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).changeRules(ruleToChange,tmp);
			graph.setAllAdded(true);
			((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).setFromGraph(true);
			userObject.setName(newRule);
			this.setId(newRule);
			this.setUserObject(userObject);	
			GraphUtilities.modifyRuleFromContainingRules(graph, rule, newRule, newObjects);	
		}
		
		d.dispose();
		d=null;
	
		
	}

	public void addModulator() {		
		
		AddReactantProductDialog d=new AddReactantProductDialog("Choose Modulator", graph);
		if(d.getChosenReactant()!=null && d.getChosenReactant()!=""){			
			
			String nm=d.getChosenReactant();
			int stoich=d.getStoichCoeff();			
			Object[] possibilities = {"forward reaction", "reverse reaction"};  					
			String s = (String)JOptionPane.showInputDialog(BiochamMainFrame.frame,
							" \nTo with reaction you want to add this modulator?\n\n",
							"Add Modulator",
							JOptionPane.PLAIN_MESSAGE,
							Icons.icons.get("question.png"),
							possibilities,
							"forward reaction");
			if(s!=null){
				if(s.equals("reverse reaction")){				
					addModulator2(nm, stoich,true);				
				}else{
					addModulator1(nm, stoich,true);				
				}	
			}
			
			/*if(GraphUtilities.getCellByName(graph,this.userObject.getReactants().get(0).getName()) instanceof EComplexCell){
				if(s.equals("reverse reaction")){					
					addModulator1(nm, stoich,true);					
				}else{
					addModulator2(nm, stoich,true);					
				}
			}else{				
				if(s.equals("reverse reaction")){				
					addModulator2(nm, stoich,true);				
				}else{
					addModulator1(nm, stoich,true);				
				}
			}*/
			nm=null;
			stoich=0;
			d.dispose();			
			d=null;
		}
		
	}
	
	

	public String getId() {
		return this.id;
	}

	public void setColor(Color green) {
		this.color=green;
		Map nested = new Hashtable();     
	    Map attributeMap = new Hashtable();
	  
	    if(graph.getContainingRules().get(this.getId())!=null){
	    	int len=graph.getContainingRules().get(this.getId()).length;
	    	for(int i=0;i<len;i++){
	    		 if(graph.getContainingRules().get(this.getId())[i]!=null){
	    			 Utils.debugMsg(graph.getContainingRules().get(this.getId())[i].getClass().toString());
	    			 Map attributeMap1 = new Hashtable();
	    			 BiochamGraphConstants.setLineColor(attributeMap1, green);			    	
	    			 BiochamGraphConstants.setBendable(attributeMap1,false);			    	
		    		 nested.put(graph.getContainingRules().get(this.getId())[i], attributeMap1);
		
		    	 }
		    }	   
	    }
	    graph.getGraphLayoutCache().edit(nested, null, null, null);
	    
	    ((MiddleEdgeAssociation)this.getMiddleEdge1()).setColor(green);
	    ((MiddleEdgeDissociation)this.getMiddleEdge2()).setColor(green);
		
	}

	public void addModulator1(String nm, int stoich, boolean fromChange) {

		
		
		String rule=this.getId();
		DefaultGraphCell cell=GraphUtilities.getCellByName(graph,nm);		
		((BiochamEntityData)cell.getUserObject()).setModulator(true);
		Object[] newObjects=new Object[2];
		
		DefaultEdge edge;
		ArrayList<UUID> molecules=new ArrayList<UUID>();
		BiochamEdgeData eData=new BiochamEdgeData(graph,molecules);
		if(stoich>1){
			eData.setStoichiometry(stoich);					
			edge = new BiochamEdge(eData);									
			BiochamGraphConstants.setLabelEnabled(edge.getAttributes(),true);
			BiochamGraphConstants.setLabelAlongEdge(edge.getAttributes(), true);
			BiochamGraphConstants.setExtraLabels(edge.getAttributes(),new Object[]{new JLabel(Integer.toString(eData.getStoichiometry()))});
			molecules.add(((BiochamEntityData)cell.getUserObject()).getId());
			eData.setMolecules(molecules);
			edge.setUserObject(eData);
		}else{
			edge = new DefaultEdge();
			BiochamGraphConstants.setSelectable(edge.getAttributes(),true);
			BiochamGraphConstants.setLineStyle(edge.getAttributes(), BiochamGraphConstants.STYLE_ORTHOGONAL);
			BiochamGraphConstants.setBendable(edge.getAttributes(), false);
		}
		
    	if(getColor()!=null){
			BiochamGraphConstants.setLineColor(edge.getAttributes(), getColor());
		}
    	
    	
    	userObject.getMolecules().add(((BiochamEntityData)cell.getUserObject()).getId());
    	this.containingMolecules=userObject.getMolecules();
    	
		BiochamObject o=new BiochamObject(cell.addPort(),stoich,((BiochamEntityData)cell.getUserObject()).getName(),cell);		
		
		if(modulators1==null){
			modulators1=new ArrayList<DefaultGraphCell>();
			GraphUtilities.setFirstModulatorLocation(graph,cell,this.getIntermediateVertexMIDDLE1().getX(),getIntermediateVertexMIDDLE1().getY());		
		}else{
			
			Map nested = new Hashtable();
			Map attributeMap1 = new Hashtable();
			Location l=getNextModulatorLocation1();
			double x=l.getX();
			double y=l.getY();			
			BiochamGraphConstants.setBounds(attributeMap1,new Rectangle2D.Double(x,y,GraphUtilities.getCellWidth(o.getInstance()),GraphUtilities.getCellHeight(o.getInstance())));			
			nested.put(o.getInstance(), attributeMap1 );
			if(graph!=null){
				graph.getGraphLayoutCache().edit(nested, null, null, null);
			}
			((BiochamEntityData)cell.getUserObject()).setPosition(new Position(x,y,0,0));
			l=null;
		}
		
		modulators1.add(cell);
		if(!userObject.getModulators1AsStrings().contains(((BiochamEntityData)cell.getUserObject()).getName())){
			userObject.getModulators1().add(new BiochamEnitity(((BiochamEntityData)cell.getUserObject()).getId(),((BiochamEntityData)cell.getUserObject()).getName()));
		}
		//userObject.getModulators1().add(new BiochamEnitity(((BiochamEntityData)cell.getUserObject()).getId(),((BiochamEntityData)cell.getUserObject()).getName()));
		edge.setSource(o.getPort());

		int p=((BiochamEntityData)o.getInstance().getUserObject()).getNumberOfConnections();
		p+=1;
		((BiochamEntityData)o.getInstance().getUserObject()).setNumberOfConnections(p);
		((BiochamEntityData)o.getInstance().getUserObject()).getInvolvedInReactions().add(rule);
		if(p>1){
			((BiochamEntityData)o.getInstance().getUserObject()).setResponsable(true);
		}
		
		this.getIntermediateVertexMIDDLE1().addPort();
		int len=getIntermediateVertexMIDDLE1().getChildCount();		
		edge.setTarget(getIntermediateVertexMIDDLE1().getChildAt(len-1));
		
		BiochamGraphConstants.setLabelEnabled(edge.getAttributes(),true);
		BiochamGraphConstants.setLabelAlongEdge(edge.getAttributes(), true);
		BiochamGraphConstants.setExtraLabels(edge.getAttributes(),new Object[]{new JLabel(Integer.toString(stoich))});
		BiochamGraphConstants.setLineEnd(edge.getAttributes(),BiochamGraphConstants.ARROW_DIAMOND);//.ARROW_INHIBITION);//.ARROW_CIRCLE
		BiochamGraphConstants.setEndFill(edge.getAttributes(),false);		
		
		graph.getGraphLayoutCache().toFront(new Object[]{edge,cell});
		
		graph.getGraphLayoutCache().insert(edge);
		graph.refresh();		
		newObjects[0]=cell; 
		newObjects[1]=edge;
		
		
		if(fromChange){

			String newRule;
			String ruleToChange;
			String tmp;
			if(rule.contains("<")){
				
				((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).deleteRule(rule,false,false,false);
				((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).setDontDraw(true,2);
				
				//delete rule
				//add each expanded rule
				//set its new name as {rule1,rule2}
				String middle=rule.substring(rule.indexOf("<")+1,rule.lastIndexOf(">")+1);
				String left=rule.substring(0,rule.indexOf("<"));
				String right=rule.substring(rule.lastIndexOf(">")+1);
				String newR="{"+left+middle+right+","+right+middle+left+"}";
				//String s="add_rules("+left+middle+right+").\n"; THE OTHER DIRECTION
				String s;
				if(reversibleDissociation){
					s="add_rules("+left+middle+right+").\nlist_molecules.\n";
				}else{
					s="add_rules("+right+middle+left+").\nlist_molecules.\n";
				}
				
				graph.getBiochamModel().sendToBiocham(s,"rules");					
				Parser_SBGNRule2BiochamRule.setFromGraph(false);
				this.setId(newR);
				rule=newR;
				userObject.setName(newR);
				
			}
			if(reversibleDissociation){
				ruleToChange=rule.substring(rule.indexOf(",")+1,rule.length()-1);
				tmp=GraphUtilities.addModulatorToBiochamRule(ruleToChange,((BiochamEntityData)cell.getUserObject()).getName());
				newRule=rule.substring(0,rule.indexOf(",")+1)+tmp+"}";
			}else{
				ruleToChange=rule.substring(1,rule.indexOf(","));
				tmp=GraphUtilities.addModulatorToBiochamRule(rule.substring(1,rule.indexOf(",")),((BiochamEntityData)cell.getUserObject()).getName());
				newRule="{"+tmp+rule.substring(rule.indexOf(","));
			}
			
			/*if(reversibleDissociation){
				ruleToChange=rule.substring(rule.indexOf(",")+1,rule.length()-1);
				tmp=GraphUtilities.addModulatorToBiochamRule(ruleToChange,((BiochamEntityData)cell.getUserObject()).getName());
				newRule=rule.substring(0,rule.indexOf(",")+1)+tmp+"}";
				
			}else{
				ruleToChange=rule.substring(1,rule.indexOf(","));
				tmp=GraphUtilities.addModulatorToBiochamRule(rule.substring(1,rule.indexOf(",")),((BiochamEntityData)cell.getUserObject()).getName());
				newRule="{"+tmp+rule.substring(rule.indexOf(","));
			}*/
			
			
							
					
			((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).setFromGraph(true);
			graph.setAllAdded(true);
			((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).changeRules(ruleToChange,tmp);
			graph.setAllAdded(true);
			((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).setFromGraph(true);
			userObject.setName(newRule);
			this.setId(newRule);
			this.setUserObject(userObject);
			GraphUtilities.modifyRuleFromContainingRules(graph, rule, newRule, newObjects);	
		}else{
			Utils.debugMsg("BEFORE contentsSize:"+graph.getContainingRules().get(rule).length);
			GraphUtilities.addObjectsToReactionContents(graph,rule,newObjects);
			Utils.debugMsg("AFTER contentsSize:"+graph.getContainingRules().get(rule).length);
		}	
	}
	
	public void addModulator2(String nm, int stoich, boolean fromChange) {

		String rule=this.getId();
		DefaultGraphCell cell=GraphUtilities.getCellByName(graph,nm);		
		((BiochamEntityData)cell.getUserObject()).setModulator(true);
		Object[] newObjects=new Object[2];
		
		DefaultEdge edge;
		ArrayList<UUID> molecules=new ArrayList<UUID>();
		BiochamEdgeData eData=new BiochamEdgeData(graph,molecules);
		if(stoich>1){
			eData.setStoichiometry(stoich);					
			edge = new BiochamEdge(eData);									
			BiochamGraphConstants.setLabelEnabled(edge.getAttributes(),true);
			BiochamGraphConstants.setLabelAlongEdge(edge.getAttributes(), true);
			BiochamGraphConstants.setExtraLabels(edge.getAttributes(),new Object[]{new JLabel(Integer.toString(eData.getStoichiometry()))});
			molecules.add(((BiochamEntityData)cell.getUserObject()).getId());
			eData.setMolecules(molecules);
			edge.setUserObject(eData);
		}else{
			edge = new DefaultEdge();
			BiochamGraphConstants.setSelectable(edge.getAttributes(),true);
			BiochamGraphConstants.setLineStyle(edge.getAttributes(), BiochamGraphConstants.STYLE_ORTHOGONAL);
			BiochamGraphConstants.setBendable(edge.getAttributes(), false);
		}
		
    	if(getColor()!=null){
			BiochamGraphConstants.setLineColor(edge.getAttributes(), getColor());
		}
    	
    	
    	userObject.getMolecules().add(((BiochamEntityData)cell.getUserObject()).getId());
    	this.containingMolecules=userObject.getMolecules();
    	
		BiochamObject o=new BiochamObject(cell.addPort(),stoich,((BiochamEntityData)cell.getUserObject()).getName(),cell);		
		
		if(modulators2==null){
			modulators2=new ArrayList<DefaultGraphCell>();
			GraphUtilities.setFirstModulatorLocation(graph,cell,this.getIntermediateVertexMIDDLE2().getX(),getIntermediateVertexMIDDLE2().getY());
		}else{
			Map nested = new Hashtable();
			Map attributeMap1 = new Hashtable();
			Location l=getNextModulatorLocation2();
			double x=l.getX();
			double y=l.getY();			
			BiochamGraphConstants.setBounds(attributeMap1,new Rectangle2D.Double(x,y,GraphUtilities.getCellWidth(o.getInstance()),GraphUtilities.getCellHeight(o.getInstance())));			
			nested.put(o.getInstance(), attributeMap1 );
			if(graph!=null){
				graph.getGraphLayoutCache().edit(nested, null, null, null);
			}
			((BiochamEntityData)cell.getUserObject()).setPosition(new Position(x,y,0,0));
			l=null;
		}
		
		modulators2.add(cell);
		if(!userObject.getModulators2AsStrings().contains(((BiochamEntityData)cell.getUserObject()).getName())){
			userObject.getModulators2().add(new BiochamEnitity(((BiochamEntityData)cell.getUserObject()).getId(),((BiochamEntityData)cell.getUserObject()).getName()));
		}
	//	userObject.getModulators2().add(new BiochamEnitity(((BiochamEntityData)cell.getUserObject()).getId(),((BiochamEntityData)cell.getUserObject()).getName()));
		edge.setSource(o.getPort());

		int p=((BiochamEntityData)o.getInstance().getUserObject()).getNumberOfConnections();
		p+=1;
		((BiochamEntityData)o.getInstance().getUserObject()).setNumberOfConnections(p);
		((BiochamEntityData)o.getInstance().getUserObject()).getInvolvedInReactions().add(rule);
		if(p>1){
			((BiochamEntityData)o.getInstance().getUserObject()).setResponsable(true);
		}
		
		this.getIntermediateVertexMIDDLE2().addPort();
		int len=getIntermediateVertexMIDDLE2().getChildCount();		
		edge.setTarget(getIntermediateVertexMIDDLE2().getChildAt(len-1));
		
		BiochamGraphConstants.setLabelEnabled(edge.getAttributes(),true);
		BiochamGraphConstants.setLabelAlongEdge(edge.getAttributes(), true);
		BiochamGraphConstants.setExtraLabels(edge.getAttributes(),new Object[]{new JLabel(Integer.toString(stoich))});
		BiochamGraphConstants.setLineEnd(edge.getAttributes(),BiochamGraphConstants.ARROW_DIAMOND);//.ARROW_INHIBITION);//.ARROW_CIRCLE
		BiochamGraphConstants.setEndFill(edge.getAttributes(),false);		
		
		graph.getGraphLayoutCache().toFront(new Object[]{edge,cell});
		
		graph.getGraphLayoutCache().insert(edge);
		graph.refresh();		
		newObjects[0]=cell; 
		newObjects[1]=edge;
		
		
		if(fromChange){

			String newRule;
			String ruleToChange;
			String tmp;
			if(rule.contains("<")){
				
				((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).deleteRule(rule,false,false,false);
				((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).setDontDraw(true,2);
				
				//delete rule
				//add each expanded rule
				//set its new name as {rule1,rule2}
				String middle=rule.substring(rule.indexOf("<")+1,rule.lastIndexOf(">")+1);
				String left=rule.substring(0,rule.indexOf("<"));
				String right=rule.substring(rule.lastIndexOf(">")+1);
				String newR="{"+left+middle+right+","+right+middle+left+"}";
				String s;
				if(reversibleDissociation){
					s="add_rules("+right+middle+left+").\nlist_molecules.\n";
				}else{
					s="add_rules("+left+middle+right+").\nlist_molecules.\n";
				}
				//String s="add_rules("+right+middle+left+").\nlist_molecules.\n"; THE OTHER DIRECTION
				graph.getBiochamModel().sendToBiocham(s,"rules");					
				Parser_SBGNRule2BiochamRule.setFromGraph(false);
				this.setId(newR);
				rule=newR;
				userObject.setName(newR);
				
			}
			if(reversibleDissociation){
				ruleToChange=rule.substring(1,rule.indexOf(","));
				tmp=GraphUtilities.addModulatorToBiochamRule(ruleToChange,((BiochamEntityData)cell.getUserObject()).getName());
				newRule="{"+tmp+rule.substring(rule.indexOf(","));
			}else{
				ruleToChange=rule.substring(rule.indexOf(",")+1,rule.length()-1);
				tmp=GraphUtilities.addModulatorToBiochamRule(ruleToChange,((BiochamEntityData)cell.getUserObject()).getName());
				newRule=rule.substring(0,rule.indexOf(",")+1)+tmp+"}";	
			}
			
			/*if(reversibleDissociation){
				ruleToChange=rule.substring(1,rule.indexOf(","));
				tmp=GraphUtilities.addModulatorToBiochamRule(rule.substring(1,rule.indexOf(",")),((BiochamEntityData)cell.getUserObject()).getName());
				newRule="{"+tmp+rule.substring(rule.indexOf(","));
				ruleToChange=rule.substring(rule.indexOf(",")+1,rule.length()-1);
				tmp=GraphUtilities.addModulatorToBiochamRule(ruleToChange,((BiochamEntityData)cell.getUserObject()).getName());
				newRule=rule.substring(0,rule.indexOf(",")+1)+tmp+"}";
				
			}else{
				ruleToChange=rule.substring(rule.indexOf(",")+1,rule.length()-1);
				tmp=GraphUtilities.addModulatorToBiochamRule(ruleToChange,((BiochamEntityData)cell.getUserObject()).getName());
				newRule=rule.substring(0,rule.indexOf(",")+1)+tmp+"}";
				ruleToChange=rule.substring(1,rule.indexOf(","));
				tmp=GraphUtilities.addModulatorToBiochamRule(rule.substring(1,rule.indexOf(",")),((BiochamEntityData)cell.getUserObject()).getName());
				newRule="{"+tmp+rule.substring(rule.indexOf(","));
			}*/
			/*String newRule;
			String ruleToChange;
			String tmp;
			if(reversibleDissociation){
				ruleToChange=rule.substring(1,rule.indexOf(","));
				tmp=GraphUtilities.addModulatorToBiochamRule(rule.substring(1,rule.indexOf(",")),((BiochamEntityData)cell.getUserObject()).getName());
				newRule="{"+tmp+rule.substring(rule.indexOf(","));
			}else{
				ruleToChange=rule.substring(rule.indexOf(",")+1,rule.length()-1);
				tmp=GraphUtilities.addModulatorToBiochamRule(rule.substring(rule.indexOf(",")+1,rule.length()-1),((BiochamEntityData)cell.getUserObject()).getName());
				newRule=rule.substring(0,rule.indexOf(",")+1)+tmp+"}";
			}*/
			
									
					
			((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).setFromGraph(true);
			graph.setAllAdded(true);
			((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).changeRules(ruleToChange,tmp);
			graph.setAllAdded(true);
			((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).setFromGraph(true);
			userObject.setName(newRule);
			this.setId(newRule);
			this.setUserObject(userObject);
			GraphUtilities.modifyRuleFromContainingRules(graph, rule, newRule, newObjects);	
		}else{
			Utils.debugMsg("BEFORE contentsSize:"+graph.getContainingRules().get(rule).length);
			GraphUtilities.addObjectsToReactionContents(graph,rule,newObjects);
			Utils.debugMsg("AFTER contentsSize:"+graph.getContainingRules().get(rule).length);
			
		}	
	}

	public ArrayList<UUID> getContainingMolecules() {
		return containingMolecules;
	}

	public void setContainingMolecules(ArrayList<UUID> containingMolecules) {
		this.containingMolecules = containingMolecules;
	}

	public BiochamGraph getGraph() {
		return graph;
	}

	public void setGraph(BiochamGraph graph) {
		this.graph = graph;
	}

	
	public ArrayList<DefaultGraphCell> getModulators1() {
		return modulators1;
	}

	public void setModulators1(ArrayList<DefaultGraphCell> modulators) {
		this.modulators1 = modulators;
	}

	public Location getNextModulatorLocation1() {
		nextModulatorLocation1=calculateNewModulatorLocation1();	
		return nextModulatorLocation1;
	}

	public Location getNextModulatorLocation2() {
		
		nextModulatorLocation2=calculateNewModulatorLocation2();		
		return nextModulatorLocation2;
	}
	
	public void setNextModulatorLocation1(Location nextModulatorLocation) {
		this.nextModulatorLocation1 = nextModulatorLocation;
	}

	public Location getNextSourceLocation() {
		nextSourceLocation=calculateNewSourceLocation();
		return nextSourceLocation;
	}

	public void setNextSourceLocation(Location nextSourceLocation) {
		this.nextSourceLocation = nextSourceLocation;
	}

	public Location getNextTargetLocation() {
		nextTargetLocation=calculateNewTargetLocation();
		return nextTargetLocation;
	}

	public void setNextTargetLocation(Location nextTargetLocation) {
		this.nextTargetLocation = nextTargetLocation;
	}
	
	public ArrayList<BiochamObject> getSources() {
		return sources;
	}

	public void setSources(ArrayList<BiochamObject> sources) {
		this.sources = sources;
	}

	public BiochamObject getTarget() {
		return target;
	}

	public void setTarget(BiochamObject target) {
		this.target = target;
	}

	public BiochamEdgeData getUserObject() {
		return userObject;
	}

	public void setUserObject(BiochamEdgeData userObject) {
		this.userObject = userObject;
	}

	public Color getColor() {
		return color;
	}

	public void setId(String id) {
		this.id = id;
	}

	public ArrayList<DefaultGraphCell> getModulators2() {
		return modulators2;
	}

	public void setModulators2(ArrayList<DefaultGraphCell> modulators2) {
		this.modulators2 = modulators2;
	}
	
	public void setNextModulatorLocation2(Location nextModulatorLocation2) {
		this.nextModulatorLocation2 = nextModulatorLocation2;
	}
	
	private Location calculateNewSourceLocation() {		
		
		int size=sources.size();
		BiochamEntityData dt=(BiochamEntityData)sources.get(size-1).getInstance().getUserObject();
		if(dt.getPosition()!=null){
			double x=dt.getPosition().getX();
			double y=dt.getPosition().getY()+(GraphUtilities.calculateMaxHeight(sources))*(sources.size()-1);			
			return new Location(x,y+10);
		}		
		return null;		
	}
	
	private Location calculateNewTargetLocation() {
		BiochamEntityData dt=(BiochamEntityData)target.getInstance().getUserObject();
		if(dt.getPosition()!=null){
			double x=dt.getPosition().getX();
			ArrayList list=new ArrayList<BiochamObject>(1);
			list.add(target);
			double y=dt.getPosition().getY()+(GraphUtilities.calculateMaxHeight(list));			
			return new Location(x,y+10);
		}		
		return null;	
	}


	private Location calculateNewModulatorLocation1() {
				
		BiochamEntityData dt=(BiochamEntityData)modulators1.get(modulators1.size()-1).getUserObject();
		if(dt.getPosition()!=null){
			double x=dt.getPosition().getX()+GraphUtilities.getCellWidth(modulators1.get(modulators1.size()-1));
			double y=dt.getPosition().getY();		
			return new Location(x+10,y);
		}		
		return null;	
	}
	
	private Location calculateNewModulatorLocation2() {
		
		BiochamEntityData dt=(BiochamEntityData)modulators2.get(modulators2.size()-1).getUserObject();
		if(dt.getPosition()!=null){
			double x=dt.getPosition().getX()+GraphUtilities.getCellWidth(modulators2.get(modulators2.size()-1));
			double y=dt.getPosition().getY();		
			return new Location(x+10,y);
		}		
		return null;	
	}
	public boolean isReversibleDissociation() {
		return reversibleDissociation;
	}
	public void setReversibleDissociation(boolean reversibleDissociation) {
		this.reversibleDissociation = reversibleDissociation;
	}
	
}
