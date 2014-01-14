package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.GraphConstants;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.modelData.ParamTableRules;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.swing.JLabel;
import javax.swing.JOptionPane;




public class StateTransition extends BiochamReaction{

	
	
	
	private DefaultGraphCell[] children;
	private BiochamGraph graph;
	IntermediateVertex intermediateVertexMIDDLE;
	IntermediateVertex2 intermediateVertexLEFT;
	IntermediateVertex3 intermediateVertexRIGHT;
	ArrayList<BiochamObject> sources, targets;
	ArrayList<DefaultGraphCell> modulators;
	BiochamEdgeData userObject;
	DefaultGraphCell modulator;
	int reversible=0;// 0=not reversible, 1=single, 2=double
	StateTransition reversibleStateTransition;
	DefaultGraphCell middleEdge;
	String id;
	Object source,target;
	ArrayList<UUID> containingMolecules;
	String leftSide,rightSide;
	Color color;
	Location nextSourceLocation, nextTargetLocation, nextModulatorLocation;
	boolean opposite=false;
	/*	 
	 * Cell is UserObject
	 * Source is a Port, Vertex.getChildAt(x)
	 * Target is a Port, Vertex.getChildAt(y)
	 * HashMap<Object,Integer> sources: Object is the Port, and Integer is the coeff. of Stoichiometry in the reaction in which this port is involved.
	 * 
	 * */	
	public StateTransition(Object cell, ArrayList<DefaultGraphCell> reacts, ArrayList<DefaultGraphCell> prods, Object object, Object object2, BiochamGraph graph, boolean reversibleSide, String rule) {
		
		super(cell);		
		ArrayList<BiochamObject> sources=new ArrayList<BiochamObject>();
		ArrayList<BiochamObject> targets=new ArrayList<BiochamObject>();		
		for(int i=0;i<reacts.size();i++){
			((BiochamEntityData)reacts.get(i).getUserObject()).setReactant(true);
			sources.add(new BiochamObject(reacts.get(i).addPort(),((BiochamEntityData)reacts.get(i).getUserObject()).getMultimerCardinality(),((BiochamEntityData)reacts.get(i).getUserObject()).getName(),reacts.get(i)));
		}
		for(int i=0;i<prods.size();i++){
			((BiochamEntityData)prods.get(i).getUserObject()).setProduct(true);
			targets.add(new BiochamObject(prods.get(i).addPort(),((BiochamEntityData)prods.get(i).getUserObject()).getMultimerCardinality(),((BiochamEntityData)prods.get(i).getUserObject()).getName(),prods.get(i)));
		}
		createTransition(cell,sources,targets,null,null,graph,reversibleSide,rule);
		
	}
	
	
	public StateTransition(Object cell,ArrayList<BiochamObject> sources, ArrayList<BiochamObject> targets,DefaultGraphCell modulator,DefaultGraphCell reversModulator,BiochamGraph graphic,boolean reversibleSide, String rule){
		
		super(cell);	 
		createTransition(cell, sources, targets, modulator, reversModulator, graphic, reversibleSide, rule);
		
	}

	
	
	/**
	 * @param cell
	 * @param sources
	 * @param targets
	 * @param modulator
	 * @param reversModulator
	 * @param graphic
	 * @param reversibleSide
	 * @param rule
	 */
	private void createTransition(Object cell, ArrayList<BiochamObject> sources, ArrayList<BiochamObject> targets, DefaultGraphCell modulator, DefaultGraphCell reversModulator, BiochamGraph graphic, boolean reversibleSide, String rule) {
		
		
		graph=graphic;
		userObject=(BiochamEdgeData) cell;	
		Position pos=GraphUtilities.checkIfOpositeDirection(graph,userObject);
		if(pos!=null){
			opposite=true;
			userObject.setHasOpposite(true);
		}else{
			opposite=false;
		}
		opposite=false;
		modulators=new ArrayList<DefaultGraphCell>();		
		containingMolecules=new ArrayList<UUID>();			
		ArrayList comps=new ArrayList();
		id=rule;
		this.sources=new ArrayList<BiochamObject>(sources);
		this.targets=new ArrayList<BiochamObject>(targets);		
		int siz=targets.size();
		for(int k=0;k<siz;k++){
			Utils.debugMsg(targets.get(k).getParentName());
		}
		children=new DefaultGraphCell[1+sources.size()+targets.size()];	
		
		DefaultGraphCell mEdge;
		mEdge=createMiddleEdge(cell,graphic);
		MiddleEdge m=(MiddleEdge)mEdge;
		this.intermediateVertexLEFT=m.getLEFTIntermediateVertex();
		this.intermediateVertexMIDDLE=m.getMIDDLEIntermediateVertex();
		this.intermediateVertexRIGHT=m.getRIGHTIntermediateVertex();
		if(opposite){
			BiochamGraphConstants.setBounds(m.getMIDDLEIntermediateVertex().getAttributes(), new Rectangle2D.Double(pos.getX(),pos.getY()-20,m.getMIDDLEIntermediateVertex().WIDTH,m.getMIDDLEIntermediateVertex().HEIGHT));
			m.getMIDDLEIntermediateVertex().setX(pos.getX());
			m.getMIDDLEIntermediateVertex().setY(pos.getY()-20);
			BiochamGraphConstants.setBounds(m.getLEFTIntermediateVertex().getAttributes(), new Rectangle2D.Double(pos.getX()-30,pos.getY()-20+5,m.getLEFTIntermediateVertex().WIDTH,m.getLEFTIntermediateVertex().HEIGHT));
			m.getLEFTIntermediateVertex().setX(pos.getX()-30);
			m.getLEFTIntermediateVertex().setY(pos.getY()-20+5);
			BiochamGraphConstants.setBounds(m.getRIGHTIntermediateVertex().getAttributes(), new Rectangle2D.Double(pos.getX()+30,pos.getY()-20+5,m.getRIGHTIntermediateVertex().WIDTH,m.getRIGHTIntermediateVertex().HEIGHT));
			m.getRIGHTIntermediateVertex().setX(pos.getX()+30);
			m.getRIGHTIntermediateVertex().setY(pos.getY()-20+5);
			
			this.intermediateVertexLEFT=m.getLEFTIntermediateVertex();
			this.intermediateVertexMIDDLE=m.getMIDDLEIntermediateVertex();
			this.intermediateVertexRIGHT=m.getRIGHTIntermediateVertex();			
		}
		children[0]=mEdge;	
		setMiddleEdge(mEdge);
		
		for(int i=0;i<sources.size();i++){
			
			BiochamEdge edge=null;							
			ArrayList<UUID> molecules=new ArrayList<UUID>();
			BiochamEdgeData eData=new BiochamEdgeData(graph,molecules);	
			int stoich=sources.get(i).getStoichiometry();
			int s2=((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getStoichoimetryIntegerForReaction();
			if(s2>stoich){
				stoich=s2;
			}
			if(stoich>1){
						
				eData.setStoichiometry(stoich);				
				edge = new BiochamEdge(eData);	
				
				BiochamGraphConstants.setLabelEnabled(edge.getAttributes(),true);
				BiochamGraphConstants.setLabelAlongEdge(edge.getAttributes(), true);
				BiochamGraphConstants.setLineStyle(edge.getAttributes(), BiochamGraphConstants.STYLE_BEZIER);
				BiochamGraphConstants.setBendable(edge.getAttributes(), true);
				BiochamGraphConstants.setRouting(edge.getAttributes(),BiochamGraphConstants.ROUTING_DEFAULT);
				//BiochamGraphConstants.setPoints(edge.getAttributes(),new ArrayList(){});
				BiochamGraphConstants.setExtraLabels(edge.getAttributes(),new Object[]{new JLabel(Integer.toString(eData.getStoichiometry()))});
				molecules.add(((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getId());
				eData.setMolecules(molecules);
				edge.setUserObject(eData);
			}else{				
				edge = new BiochamEdge(eData);
				BiochamGraphConstants.setSelectable(edge.getAttributes(),true);
				BiochamGraphConstants.setLineStyle(edge.getAttributes(), BiochamGraphConstants.STYLE_BEZIER);
				BiochamGraphConstants.setRouting(edge.getAttributes(),BiochamGraphConstants.ROUTING_DEFAULT);
				BiochamGraphConstants.setBendable(edge.getAttributes(), true);
				
			}			
			if(reversibleSide){
				BiochamGraphConstants.setLineBegin(edge.getAttributes(), BiochamGraphConstants.ARROW_CLASSIC);
				BiochamGraphConstants.setBeginFill(edge.getAttributes(), true);
			}
			
			int p=((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getNumberOfConnections();
			p+=1;
			((BiochamEntityData)sources.get(i).getInstance().getUserObject()).setNumberOfConnections(p);
			((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getInvolvedInReactions().add(rule);
			if(p>1){
				((BiochamEntityData)sources.get(i).getInstance().getUserObject()).setResponsable(true);
			}
			
			
			edge.setSource(sources.get(i).getPort());			
			comps.add(sources.get(i).getInstance());	
			
			if(opposite){
				edge.setTarget(((MiddleEdge)mEdge).getRIGHTIntermediateVertex().addPort());
			}else{
				edge.setTarget(((MiddleEdge)mEdge).getLEFTIntermediateVertex().addPort());
			}
			
			children[i+1] = edge;
			comps.add(edge);
		}
		
		comps.add(mEdge);
		
		for(int i=0;i<targets.size();i++){
			
			BiochamEdge edge3;
			BiochamEdgeData eData=null;
			ArrayList<UUID> molecules=new ArrayList<UUID>();	
			eData=new BiochamEdgeData(graph,molecules);
			if(targets.get(i).getStoichiometry()>1){
				
				eData.setStoichiometry(targets.get(i).getStoichiometry());				
				edge3 = new BiochamEdge(eData);	
				BiochamGraphConstants.setLineStyle(edge3.getAttributes(), BiochamGraphConstants.STYLE_BEZIER);
				BiochamGraphConstants.setRouting(edge3.getAttributes(),BiochamGraphConstants.ROUTING_DEFAULT);
				BiochamGraphConstants.setBendable(edge3.getAttributes(), true);
				//BiochamGraphConstants.setLineStyle(edge3.getAttributes(), BiochamGraphConstants.STYLE_SPLINE);
				BiochamGraphConstants.setBendable(edge3.getAttributes(), true);
				BiochamGraphConstants.setLabelEnabled(edge3.getAttributes(),true);
				BiochamGraphConstants.setLabelAlongEdge(edge3.getAttributes(), true);		
				//BiochamGraphConstants.setRouting(edge3.getAttributes(),BiochamGraphConstants.ROUTING_DEFAULT);
				BiochamGraphConstants.setExtraLabels(edge3.getAttributes(),new Object[]{new JLabel(Integer.toString(eData.getStoichiometry()))});
				molecules.add(((BiochamEntityData)targets.get(i).getInstance().getUserObject()).getId());
				eData.setMolecules(molecules);
				edge3.setUserObject(eData);
			}else{				
				edge3 = new BiochamEdge(eData);	
				BiochamGraphConstants.setSelectable(edge3.getAttributes(),true);
				BiochamGraphConstants.setRouting(edge3.getAttributes(),BiochamGraphConstants.ROUTING_DEFAULT);
				BiochamGraphConstants.setLineStyle(edge3.getAttributes(), BiochamGraphConstants.STYLE_BEZIER);
				BiochamGraphConstants.setBendable(edge3.getAttributes(), true);
				
			}
			//if(reversibleSide){
			BiochamGraphConstants.setLineEnd(edge3.getAttributes(), BiochamGraphConstants.ARROW_CLASSIC);
			BiochamGraphConstants.setEndFill(edge3.getAttributes(), true);
			//}
			if(opposite){
				edge3.setSource(((MiddleEdge)mEdge).getLEFTIntermediateVertex().addPort());
			}else{
				edge3.setSource(((MiddleEdge)mEdge).getRIGHTIntermediateVertex().addPort());
			}
			
			
						
			int p=((BiochamEntityData)targets.get(i).getInstance().getUserObject()).getNumberOfConnections();
			p+=1;
			((BiochamEntityData)targets.get(i).getInstance().getUserObject()).setNumberOfConnections(p);
			((BiochamEntityData)targets.get(i).getInstance().getUserObject()).getInvolvedInReactions().add(rule);			
			if(p>1){
				((BiochamEntityData)targets.get(i).getInstance().getUserObject()).setResponsable(true);
			}
			
			edge3.setTarget(targets.get(i).getPort());
			comps.add(targets.get(i).getInstance());
			children[sources.size()+1+i] = edge3;
			comps.add(edge3);
			
		}		
	
		if(!opposite){			
			GraphUtilities.setSourcesLocation(graph,sources,intermediateVertexLEFT.getX()-20,intermediateVertexLEFT.getY());
			GraphUtilities.setTargetsLocation(graph,targets,intermediateVertexRIGHT.getX(),intermediateVertexRIGHT.getY());
		}
		
		
		
		BiochamGraphConstants.setEditable(this.getAttributes(),true);
		BiochamGraphConstants.setDisconnectable(this.getAttributes(),false);
		BiochamGraphConstants.setGroupOpaque(this.getAttributes(),true);
		BiochamGraphConstants.setMoveable(this.getAttributes(),true);
		BiochamGraphConstants.setSelectable(this.getAttributes(),true);
		BiochamGraphConstants.setChildrenSelectable(this.getAttributes(),true);
		graphic.setDisconnectOnMove(false);
		graphic.setEdgeLabelsMovable(true);
		graphic.setOpaque(true);		
		graphic.getGraphLayoutCache().toBack(children);
		graphic.getGraphLayoutCache().insertGroup(this,children);
		
		
		graphic.getContainingRules().put(rule.trim(),comps.toArray());
		graph=graphic;		
		comps=null;
		//GraphUtilities.setLineStyleOrthogonalSelectedCells(graph);		
		userObject.setName(id);
		this.setUserObject(userObject);
		cell=userObject;
		super.setUserObject(userObject);
		containingMolecules=userObject.getMolecules();
		//graphic.updateReaction(rule, null);
		
		if(!opposite){
			((MiddleEdge)getMiddleEdge()).setHorizontal();
		}
		
		//GraphUtilities.setLineStyleOrthogonalSelectedCells(graph);
	}


	
	
	public void addReactant() {
		
		String rule=this.getId();		
		AddReactantProductDialog d=new AddReactantProductDialog("Choose Reactant", graph);
		Object[] newObjects=new Object[2];
		
		if(d.getChosenReactant()!=null && d.getChosenReactant()!=""){
		
			
			DefaultGraphCell cell=GraphUtilities.getCellByName(graph,d.getChosenReactant().trim());
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
			if(reversible>0){					
				BiochamGraphConstants.setLineBegin(edge.getAttributes(), BiochamGraphConstants.ARROW_CLASSIC);
				BiochamGraphConstants.setBeginFill(edge.getAttributes(), true);						
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
			
			edge.setSource(o.getPort());
			if(opposite){
				getRIGHTIntermediateVertex().addPort();
				int len=getRIGHTIntermediateVertex().getChildCount();
				edge.setTarget(getRIGHTIntermediateVertex().getChildAt(len-1));
			}else{
				getLEFTIntermediateVertex().addPort();
				int len=getLEFTIntermediateVertex().getChildCount();
				edge.setTarget(getLEFTIntermediateVertex().getChildAt(len-1));
			}
			
			graph.getGraphLayoutCache().toFront(new Object[]{edge,cell});			
			graph.getGraphLayoutCache().insert(edge);
			graph.refresh();
			newObjects[0]=cell;
			newObjects[1]=edge;
			sources.add(o);
			
			String newRule=GraphUtilities.addReactantToBiochamRule(rule,((BiochamEntityData)cell.getUserObject()).getName());			
			
			//graph.updateReaction(rule, newRule);
			((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).setFromGraph(true);
			graph.setAllAdded(true);
			((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).changeRules(rule,newRule);
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
		
		
		String rule=this.getName();					
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
			if(opposite){
				getLEFTIntermediateVertex().addPort();
				int len=getLEFTIntermediateVertex().getChildCount();
				edge.setSource(getLEFTIntermediateVertex().getChildAt(len-1));
			}else{
				getRIGHTIntermediateVertex().addPort();
				int len=getRIGHTIntermediateVertex().getChildCount();
				edge.setSource(getRIGHTIntermediateVertex().getChildAt(len-1));
			}
			
							
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
			
		
			edge.setTarget(o.getPort());
			
			int p=((BiochamEntityData)o.getInstance().getUserObject()).getNumberOfConnections();
			p+=1;
			((BiochamEntityData)o.getInstance().getUserObject()).setNumberOfConnections(p);
			((BiochamEntityData)o.getInstance().getUserObject()).getInvolvedInReactions().add(rule);
			if(p>1){
				((BiochamEntityData)o.getInstance().getUserObject()).setResponsable(true);
			}
			
			GraphConstants.setLineEnd(edge.getAttributes(), BiochamGraphConstants.ARROW_CLASSIC);
			GraphConstants.setEndFill(edge.getAttributes(), true);	
			
			graph.getGraphLayoutCache().toFront(new Object[]{edge,cell});			
			graph.getGraphLayoutCache().insert(edge);
			graph.refresh();
			targets.add(o);
			newObjects[0]=cell; 
			newObjects[1]=edge;
			
			String newRule=GraphUtilities.addProductToBiochamRule(rule,((BiochamEntityData)cell.getUserObject()).getName());		
				
			//graph.updateReaction(rule, newRule);
			((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).setFromGraph(true);
			graph.setAllAdded(true);
			((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).changeRules(rule,newRule);
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
			addModulator(d.getChosenReactant(), d.getStoichCoeff(),true);		
		}
		d.dispose();
		d=null;
	}


	
	public void addModulator(String nm, int stoich,boolean fromChange) {
	
		String rule=this.getName();		
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
		
		if(modulators.size()==0){
			GraphUtilities.setFirstModulatorLocation(graph,cell,getMIDDLEIntermediateVertex().getX(),getMIDDLEIntermediateVertex().getY());
		}else{
			Map nested = new Hashtable();
			Map attributeMap1 = new Hashtable();
			Location l=getNextModulatorLocation();
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
		if(!modulators.contains(cell)){
			modulators.add(cell);	
		}
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
		
		boolean cont=true;
		if(userObject.isReversible() && userObject.getReversibilityType()==ReversibilityType.DOUBLE){
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
						getMIDDLEIntermediateVertex2().addPort();
						edge.setTarget(getMIDDLEIntermediateVertex2().getChildAt(getMIDDLEIntermediateVertex2().getChildCount()-1));
					}else{
						getMIDDLEIntermediateVertex().addPort();
						int len=getMIDDLEIntermediateVertex().getChildCount();		
						edge.setTarget(getMIDDLEIntermediateVertex().getChildAt(len-1));
					}
				}else{
					cont=false;
				}
				
		}else{
			getMIDDLEIntermediateVertex().addPort();
			int len=getMIDDLEIntermediateVertex().getChildCount();		
			edge.setTarget(getMIDDLEIntermediateVertex().getChildAt(len-1));
		}		
		if(cont){
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

				String newRule=GraphUtilities.addModulatorToBiochamRule(rule,((BiochamEntityData)cell.getUserObject()).getName());	
				
				//graph.updateReaction(rule, newRule);
				((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).setFromGraph(true);
				graph.setAllAdded(true);
				((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).changeRules(rule,newRule);
				graph.setAllAdded(true);
				((ParamTableRules)graph.getBiochamModel().getRules().getParamTable()).setFromGraph(true);
				userObject.setName(newRule);
				this.setId(newRule);
				this.setUserObject(userObject);
				GraphUtilities.modifyRuleFromContainingRules(graph, rule, newRule, newObjects);	
			}else{
				GraphUtilities.addObjectsToReactionContents(graph, rule, newObjects);
			}	
		}
		
	}
	


	public BiochamGraph getGraph() {
		return graph;
	}


	public void setGraph(BiochamGraph graph) {
		this.graph = graph;
	}


	public StateTransition getReversibleStateTransition() {
		return reversibleStateTransition;
	}


	public void setReversibleStateTransition(
			StateTransition reversibleStateTransition) {
		this.reversibleStateTransition = reversibleStateTransition;
	}


	public DefaultGraphCell getMiddleEdge() {
		return middleEdge;
	}


	public void setMiddleEdge(DefaultGraphCell middleEdge) {
		this.middleEdge = middleEdge;		
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
		return new MiddleEdge(cell,graphic,x,y);
	}


	private DefaultGraphCell createReversibleMiddleEdge(Object cell,BiochamGraph graphic) {
		
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
		return new ReversibleMiddleEdge(cell,graphic,x,y);
	}


	public DefaultGraphCell[] getItsChildren(){
		return children;
	}
	
	public IntermediateVertex2 getLEFTIntermediateVertex() {	
		return ((MiddleEdge)middleEdge).getLEFTIntermediateVertex();
	}


	public IntermediateVertex getMIDDLEIntermediateVertex() {
		return ((MiddleEdge)middleEdge).getMIDDLEIntermediateVertex();
	}
	public IntermediateVertex getMIDDLEIntermediateVertex2() {
		if(reversible==2){
			return ((ReversibleMiddleEdge)middleEdge).getIntermediateVertexMIDDLE2();
		}else{
			return null;
		}
	}	

	public IntermediateVertex3 getRIGHTIntermediateVertex() {
		return ((MiddleEdge)middleEdge).getRIGHTIntermediateVertex();
	}

	public ArrayList<BiochamObject> getSources() {
		return sources;
	}

	public void setSources(ArrayList<BiochamObject> sources) {
		this.sources = sources;
	}

	public ArrayList<BiochamObject> getTargets() {
		return targets;
	}

	public void setTargets(ArrayList<BiochamObject> targets) {
		this.targets = targets;
	}
	
	public String toString(){
		return "";
	}

	public DefaultGraphCell getModulator() {
		return modulator;
	}

	public void setModulator(DefaultGraphCell modulator) {
		this.modulator = modulator;
	}
	public String getName() {		
		return id;
	}

	public String getId() {
		return this.id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public ArrayList<UUID> getContainingMolecules() {
		return containingMolecules;
	}

	public void setContainingMolecules(ArrayList<UUID> molecules) {
		this.containingMolecules = molecules;
	}
	public Object getSource(){
		return source;
	}	
	public Object getTarget(){
		return target;
	}


	public void setColor(Color green) {
		
		
		this.color=green;
		   Map nested = new Hashtable();     
		    Map attributeMap = new Hashtable();
		    for(Object o:graph.getRoots()) {
		      //thcikness only for for edges
		      //if(o instanceof DefaultEdge) {
		    	if(o instanceof DefaultEdge || o instanceof BiochamEdge){		    		
		    		BiochamGraphConstants.setBendable(attributeMap,false);
		    		BiochamGraphConstants.setRouting(attributeMap, BiochamGraphConstants.ROUTING_DEFAULT);
		    		BiochamGraphConstants.setLineStyle(attributeMap, BiochamGraphConstants.STYLE_ORTHOGONAL);
		    		nested.put(o, attributeMap);
		    	}
		    	
		      //}    
		    }
		    
		    for(int i=0;i<graph.getContainingRules().get(this.getId()).length;i++){
		    	 Map attributeMap1 = new Hashtable();
		    	 BiochamGraphConstants.setLineColor(attributeMap1, green);
		    	 BiochamGraphConstants.setBendable(attributeMap1,false);
		    	 if(graph.getContainingRules().get(this.getId())[i]!=null){
		    		 nested.put(graph.getContainingRules().get(this.getId())[i], attributeMap1);
		    	 }
		    }	
		    nested.put(this, attributeMap);
		    
		    
		    if(this.getMiddleEdge() instanceof MiddleEdge){
		    	((MiddleEdge)this.getMiddleEdge()).setColor(green);
		    }else if(this.getMiddleEdge() instanceof ReversibleMiddleEdge){
		    	((ReversibleMiddleEdge)this.getMiddleEdge()).setColor(green);
		    }
		    
		    
		   
		    graph.getGraphLayoutCache().edit(nested, null, null, null);
		    
		BiochamGraphConstants.setLineColor(this.getAttributes(), green);
		
	}


	public Color getColor() {
		return color;
	}


	public Location getNextModulatorLocation() {
		
		nextModulatorLocation=calculateNewModulatorLocation();		
		return nextModulatorLocation;
	}

	public void setNextModulatorLocation(Location nextModulatorLocation) {
		this.nextModulatorLocation = nextModulatorLocation;
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
		BiochamEntityData dt=(BiochamEntityData)targets.get(targets.size()-1).getInstance().getUserObject();
		if(dt.getPosition()!=null){
			double x=dt.getPosition().getX();
			double y=dt.getPosition().getY()+(GraphUtilities.calculateMaxHeight(targets))*(targets.size()-1);			
			return new Location(x,y+10);
		}		
		return null;	
	}


	private Location calculateNewModulatorLocation() {
				
		BiochamEntityData dt=(BiochamEntityData)modulators.get(modulators.size()-1).getUserObject();
		if(dt.getPosition()!=null){
			double x=dt.getPosition().getX()+GraphUtilities.getCellWidth(modulators.get(modulators.size()-1));
			double y=dt.getPosition().getY();		
			return new Location(x+10,y);
		}		
		return null;	
	}

	

	public boolean isOpposite() {
		return opposite;
	}


	public void setOpposite(boolean opposite) {
		this.opposite = opposite;
	}


	public void changeLhs_Rhs() {
				
	}
	
	public void setPosition(double x, double y){
		
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(x,y,getMIDDLEIntermediateVertex().WIDTH,getMIDDLEIntermediateVertex().HEIGHT));
		nested.put(getMIDDLEIntermediateVertex(), attributeMap1);	
		getMIDDLEIntermediateVertex().setX(x);
		getMIDDLEIntermediateVertex().setY(y);
		getMIDDLEIntermediateVertex().setNewX(x);
		getMIDDLEIntermediateVertex().setNewY(y);
		
		attributeMap1 = new Hashtable();
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(x+30,y+5,getRIGHTIntermediateVertex().WIDTH,getRIGHTIntermediateVertex().HEIGHT));
		nested.put(getRIGHTIntermediateVertex(), attributeMap1);
		getRIGHTIntermediateVertex().setX(x+50);
		getRIGHTIntermediateVertex().setY(y+5);
		
		attributeMap1 = new Hashtable();	
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(x-30,y+5,getLEFTIntermediateVertex().WIDTH,getLEFTIntermediateVertex().HEIGHT));
		nested.put(getLEFTIntermediateVertex(), attributeMap1);
		getLEFTIntermediateVertex().setX(x-50);
		getLEFTIntermediateVertex().setY(y+5);
			
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested);
		}	
		graph.refresh();
	}
	
	
	
	public void setPosition(double x, double y, boolean begin){
		
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(x,y,getMIDDLEIntermediateVertex().WIDTH,getMIDDLEIntermediateVertex().HEIGHT));
		nested.put(getMIDDLEIntermediateVertex(), attributeMap1);	
		getMIDDLEIntermediateVertex().setX(x);
		getMIDDLEIntermediateVertex().setY(y);
		getMIDDLEIntermediateVertex().setNewX(x);
		getMIDDLEIntermediateVertex().setNewY(y);
		
		attributeMap1 = new Hashtable();
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(x+30,y+5,getRIGHTIntermediateVertex().WIDTH,getRIGHTIntermediateVertex().HEIGHT));
		nested.put(getRIGHTIntermediateVertex(), attributeMap1);
		getRIGHTIntermediateVertex().setX(x+50);
		getRIGHTIntermediateVertex().setY(y+5);
		
		attributeMap1 = new Hashtable();	
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(x-30,y+5,getLEFTIntermediateVertex().WIDTH,getLEFTIntermediateVertex().HEIGHT));
		nested.put(getLEFTIntermediateVertex(), attributeMap1);
		getLEFTIntermediateVertex().setX(x-50);
		getLEFTIntermediateVertex().setY(y+5);
			
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested);
		}		
		if(sources.size()>1){
			/*System.out.println("BEFORE:\n");
			for(int o=0;o<sources.size();o++){
				System.out.println(sources.get(o).getParentName());
			}*/
			sources=GraphUtilities.sortMoleculesByInvolvedIn(graph,sources);
			/*System.out.println("AFTER:\n");
			for(int o=0;o<sources.size();o++){
				System.out.println(sources.get(o).getParentName());
			}*/
		}
		
		/*ArrayList<BiochamObject> sortedList=new ArrayList<BiochamObject>(sources.size());	
		DefaultGraphCell d=null;
		int num=1;
		int max=0;		
		HashMap<Integer,BiochamObject> sortedSources=new HashMap<Integer,BiochamObject>();
		for(int i=0;i<sources.size();i++){
			d=GraphUtilities.getCellByName(graph,sources.get(i).getParentName());
			num=GraphUtilities.getBiochamEntityDataFromCell(d).getInvolvedInReactions().size();
			sortedSources.put(num, sources.get(i));
		}
		int siz0=sortedSources.size();
		List<Integer> keys=new ArrayList(sortedSources.keySet());
		int siz1=keys.size();
		Arrays.sort(keys.toArray());
		int siz2=keys.size();
		
		for(Iterator it=keys.iterator();it.hasNext();)   {  
	        // Map.Entry entry =(Map.Entry)it.next(); 
	         int key =(Integer)it.next();
	         BiochamObject value=(BiochamObject)sortedSources.get(key);
	         System.out.println(key+":"+value.getParentName());
	         System.out.println("\n");       
		}*/
		/*while(sortedList.size()<sources.size()){
			for(int i=0;i<sources.size();i++){
				d=GraphUtilities.getCellByName(graph,sources.get(i).getParentName());
				num=GraphUtilities.getBiochamEntityDataFromCell(d).getInvolvedInReactions().size();
				if(num<=1){
					sortedList.add(sources.get(i));
				}
				if(max<num){
					max=num;
				}
			}
			for(int i=0;i<sources.size();i++){
				d=GraphUtilities.getCellByName(graph,sources.get(i).getParentName());
				num=GraphUtilities.getBiochamEntityDataFromCell(d).getInvolvedInReactions().size();
				if(num<=1){
					sortedList.add(sources.get(i));
				}
			}
		}
		for(int k=0;k<sources.size();k++){
			d=GraphUtilities.getCellByName(graph,sources.get(k).getParentName());
			if(d!=null){
				if(!GraphUtilities.getBiochamEntityDataFromCell(d).isResponsable()){
					sortedList.add(sources.get(k));
				}		
			}
			d=null;
		}
		for(int i=0;i<sources.size();i++){
			if(!sortedList.contains(sources.get(i))){
				sortedList.add(sources.get(i));
			}
		}*/
		//DefaultGraphCell d=null;
			GraphUtilities.setSourcesLocation(graph,sources,x-50,y+5,true);	
			for(int i=0;i<sources.size();i++){
				((BiochamEntityData)sources.get(i).getInstance().getUserObject()).setSetByAlgo(true);				
			}
	//	}		
		/*ArrayList<BiochamObject> rest=new ArrayList<BiochamObject>();
		for(int i=0;i<sources.size();i++){
			if(!((BiochamEntityData)sources.get(i).getInstance().getUserObject()).isSetByAlgo()){
				rest.add(sources.get(i));					
			}
		}
		for(int i=0;i<rest.size();i++){
			int indOfPrev=sources.indexOf(rest.get(i));
			if(indOfPrev==0){
				indOfPrev+=1;
			}
			GraphUtilities.setSourcesLocation(graph,sources,x-50,y+5);
		}*/
		/*int siz=targets.size();
		for(int i=0;i<siz;i++){
			System.out.println(targets.get(i).getParentName());
		}
		System.out.println("BEFORE:\n");
		for(int o=0;o<targets.size();o++){
			System.out.println(targets.get(o).getParentName());
		}
		sortedList=new ArrayList<BiochamObject>(targets.size());		
		
		for(int k=0;k<targets.size();k++){
			d=GraphUtilities.getCellByName(graph,targets.get(k).getParentName());
			if(d!=null){
				if(!GraphUtilities.getBiochamEntityDataFromCell(d).isResponsable()){
					sortedList.add(targets.get(k));
				}		
			}
			d=null;
		}
		for(int i=0;i<targets.size();i++){
			if(!sortedList.contains(targets.get(i))){
				sortedList.add(targets.get(i));
			}
		}
		d=null;
		System.out.println("AFTER:\n");
		for(int o=0;o<sortedList.size();o++){
			System.out.println(sortedList.get(o).getParentName());
		}*/
		if(targets.size()>1){
			targets=GraphUtilities.sortMoleculesByInvolvedIn(graph,targets);	
		}		
		GraphUtilities.setTargetsLocation(graph,targets,x+30,y+5);
		for(int o=0;o<targets.size();o++){
			try{
				((BiochamEntityData)targets.get(o).getInstance().getUserObject()).setSetByAlgo(true);
			}catch(Exception e){}
		}
		graph.refresh();
		
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
}
