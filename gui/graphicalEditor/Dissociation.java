package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.GraphConstants;

import fr.inria.contraintes.biocham.modelData.ParamTableRules;

import java.awt.Color;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import javax.swing.JLabel;
import javax.swing.JOptionPane;




public class Dissociation extends BiochamReaction{
	
		
		
	IntermediateVertexDissoc intermediateVertexMIDDLE;
	IntermediateVertex2 intermediateVertexLEFT=null;
	IntermediateVertex3 intermediateVertexRIGHT=null;
	//IntermediateVertexDissoc dissocConnection=null;	
	ArrayList<BiochamObject> targets;
	ArrayList<DefaultGraphCell> modulators;
	ArrayList<UUID> containingMolecules;
	DefaultGraphCell[] children;	
	BiochamGraph graph;
	BiochamEdgeData userObject;
	BiochamObject source;
	DefaultGraphCell modulator;	
	String id;
	String leftSide,rightSide;
	Color color;
	Location nextTargetLocation, nextModulatorLocation;
	DefaultGraphCell middleEdge;
	boolean opposite=false;
	

	//FROM FILE.....
	public Dissociation(Object cell, DefaultGraphCell react, ArrayList<DefaultGraphCell> prods, Object object, BiochamGraph graph, String rule) {		
		super(cell);	
		ArrayList<BiochamObject> targetss=new ArrayList<BiochamObject>();	
		for(int i=0;i<prods.size();i++){
			((BiochamEntityData)prods.get(i).getUserObject()).setProduct(true);
			targetss.add(new BiochamObject(prods.get(i).addPort(),((BiochamEntityData)prods.get(i).getUserObject()).getMultimerCardinality(),((BiochamEntityData)prods.get(i).getUserObject()).getName(),prods.get(i)));
		}
		((BiochamEntityData)react.getUserObject()).setReactant(true);
		createDissociation(cell, new BiochamObject(react.addPort(),((BiochamEntityData)react.getUserObject()).getMultimerCardinality(),((BiochamEntityData)react.getUserObject()).getName(),react), targetss, null, graph, rule);
	}
		 
	/* 
	 * Cell is UserObject
	 * Source is a Port, Vertex.getChildAt(x)
	 * Target is a Port, Vertex.getChildAt(y)
	 * */
	public Dissociation(Object cell,BiochamObject source,ArrayList<BiochamObject> targets,DefaultGraphCell modulator,BiochamGraph graphic,String rule){			
		super(cell);		
		createDissociation(cell, source, targets, modulator, graphic, rule);		
	}

	
	
	
	/**
	 * @param cell
	 * @param source
	 * @param targets
	 * @param modulator
	 * @param graphic
	 * @param rule
	 */
	private void createDissociation(Object cell, BiochamObject source, ArrayList<BiochamObject> targets, DefaultGraphCell modulator, BiochamGraph graphic, String rule) {
		
		
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
		if(modulator!=null){
			modulators.add(modulator);
		}
		containingMolecules=new ArrayList<UUID>();
		userObject=(BiochamEdgeData) cell;
		userObject.setStoichiometry(source.getStoichiometry());
		ArrayList comps=new ArrayList();		
		id=rule;	
		
		this.targets=new ArrayList<BiochamObject>(targets);
		this.source=new BiochamObject(source.getInstance().addPort(),source.getStoichiometry(),((BiochamEntityData)source.getInstance().getUserObject()).getName(),source.getInstance());
		
		int size=2+targets.size();
		children=new DefaultGraphCell[size];
		
		
		
		DefaultGraphCell mEdge;
		mEdge=createMiddleEdge(cell,graphic);
		MiddleEdgeDissociation m=(MiddleEdgeDissociation)mEdge;
		//this.intermediateVertexLEFT=m.getLEFTIntermediateVertex();
		this.intermediateVertexMIDDLE=m.getMIDDLEIntermediateVertex();
		
		if(opposite){
			BiochamGraphConstants.setBounds(m.getMIDDLEIntermediateVertex().getAttributes(), new Rectangle2D.Double(pos.getX(),pos.getY()-20,m.getMIDDLEIntermediateVertex().WIDTH,m.getMIDDLEIntermediateVertex().HEIGHT));
			m.getMIDDLEIntermediateVertex().setX(pos.getX());
			m.getMIDDLEIntermediateVertex().setY(pos.getY()-20);
			BiochamGraphConstants.setBounds(m.getLEFTIntermediateVertex().getAttributes(), new Rectangle2D.Double(pos.getX()-30,pos.getY()-20+5,m.getLEFTIntermediateVertex().WIDTH,m.getLEFTIntermediateVertex().HEIGHT));
			m.getLEFTIntermediateVertex().setX(pos.getX()-30);
			m.getLEFTIntermediateVertex().setY(pos.getY()-20+5);
						
			this.intermediateVertexMIDDLE=m.getMIDDLEIntermediateVertex();
			this.intermediateVertexLEFT=m.getLEFTIntermediateVertex();			
		}else{
			this.intermediateVertexRIGHT=m.getRIGHTIntermediateVertex();//getLEFTIntermediateVertex
		}
		children[0]=mEdge;	
		setMiddleEdge(mEdge);
		//children[0]=mEdge;				
		//children[0].addPort();						
		comps.add(mEdge);
		/*new IntermediateVertexNode(cell);
		intermediateVertexMIDDLE=new IntermediateVertex(graphic,cell);	
		children[0]=intermediateVertexMIDDLE;	
		comps.add(intermediateVertexMIDDLE);
		children[0].addPort();
		children[0].addPort();*/

		DefaultEdge edge = new DefaultEdge();
		 BiochamGraphConstants.setSelectable(edge.getAttributes(),true);
		 BiochamGraphConstants.setLineStyle(edge.getAttributes(), BiochamGraphConstants.STYLE_ORTHOGONAL);
		 BiochamGraphConstants.setBendable(edge.getAttributes(), false);
		 
		edge.setSource(source.getInstance().addPort());
		int p=((BiochamEntityData)source.getInstance().getUserObject()).getNumberOfConnections();
		p+=1;
		((BiochamEntityData)source.getInstance().getUserObject()).setNumberOfConnections(p);
		((BiochamEntityData)source.getInstance().getUserObject()).getInvolvedInReactions().add(rule);
		if(p>1){
			((BiochamEntityData)source.getInstance().getUserObject()).setResponsable(true);
		}
		containingMolecules.add(((BiochamEntityData)source.getInstance().getUserObject()).getId());
		comps.add(source.getInstance());
		edge.setTarget(((MiddleEdgeDissociation)mEdge).getMIDDLEIntermediateVertex().addPort());
		//edge.setTarget(children[0].getChildAt(0));
		children[1] = edge;
		comps.add(edge);
		
		/*new IntermediateVertexDissocNode(cell);
		dissocConnection=new IntermediateVertexDissoc(intermediateVertexMIDDLE,cell);	
		children[2]=dissocConnection;
		comps.add(dissocConnection);
		children[2].addPort();	*/
		
		
		/*DefaultEdge edge1 = new DefaultEdge();
		edge1.setSource(children[0].getChildAt(1));			
		edge1.setTarget(children[2].getChildAt(0));		
		children[3] = edge1;
		comps.add(edge1);
				
		int sz=targets.size();*/
		
		if(targets.size()==1){
			
			//userObject
			
			//children[2].addPort();
			DefaultEdge e;
			ArrayList<UUID> molecules=new ArrayList<UUID>();
			BiochamEdgeData eData=new BiochamEdgeData(graph,molecules);
			int stoich1=targets.get(0).getStoichiometry();
			int stoich=((BiochamEntityData)targets.get(0).getInstance().getUserObject()).getStoichoimetryIntegerForReaction();
			if(stoich>1 || stoich1>1){		
				if(stoich>1){
					eData.setStoichiometry(stoich);
				}else{
					eData.setStoichiometry(stoich1);
				}
							
				e = new BiochamEdge(eData);					
				BiochamGraphConstants.setLabelAlongEdge(e.getAttributes(), true);
				BiochamGraphConstants.setLabelEnabled(e.getAttributes(),true);
				BiochamGraphConstants.setExtraLabels(e.getAttributes(),new Object[]{new JLabel(Integer.toString(eData.getStoichiometry()))});
				molecules.add(((BiochamEntityData)targets.get(0).getInstance().getUserObject()).getId());	
				eData.setMolecules(molecules);
				edge.setUserObject(eData);
			}else{
				 e = new DefaultEdge();
				 BiochamGraphConstants.setSelectable(e.getAttributes(),true);
				 BiochamGraphConstants.setLineStyle(e.getAttributes(), BiochamGraphConstants.STYLE_ORTHOGONAL);
				 BiochamGraphConstants.setBendable(e.getAttributes(), false);
				
			}
			GraphConstants.setLineEnd(e.getAttributes(), BiochamGraphConstants.ARROW_CLASSIC);
			GraphConstants.setEndFill(e.getAttributes(), true);
			if(opposite){
				e.setSource(((MiddleEdgeDissociation)mEdge).getLEFTIntermediateVertex().addPort());
			}else{
				e.setSource(((MiddleEdgeDissociation)mEdge).getRIGHTIntermediateVertex().addPort());
			}
			
			e.setTarget(targets.get(0).getPort());		
			p=((BiochamEntityData)targets.get(0).getInstance().getUserObject()).getNumberOfConnections();
			p+=1;
			((BiochamEntityData)targets.get(0).getInstance().getUserObject()).setNumberOfConnections(p);
			((BiochamEntityData)targets.get(0).getInstance().getUserObject()).getInvolvedInReactions().add(rule);
			if(p>1){
				((BiochamEntityData)targets.get(0).getInstance().getUserObject()).setResponsable(true);
			}
			
			comps.add(targets.get(0).getInstance());
			children[2] = e;	
			comps.add(e);
			
			
		}else{
			// n -times.....
			for(int i=0;i<targets.size();i++){
				//children[2].addPort();
				DefaultEdge e;
				ArrayList<UUID> molecules=new ArrayList<UUID>();
				BiochamEdgeData eData=new BiochamEdgeData(graph,molecules);
				int stoich=1;
				if(((BiochamEntityData)source.getInstance().getUserObject()).getContainingMolecules().size()>0){
					for(int j=0;j<((BiochamEntityData)source.getInstance().getUserObject()).getContainingMolecules().size();j++){
						if(((BiochamEntityData)source.getInstance().getUserObject()).getContainingMolecules().get(j).getReactionStoichiometry()>1){
							stoich=((BiochamEntityData)source.getInstance().getUserObject()).getContainingMolecules().get(j).getReactionStoichiometry();
						}else if(((BiochamEntityData)source.getInstance().getUserObject()).getContainingMolecules().get(j).getCardinality()>1){
							stoich=((BiochamEntityData)source.getInstance().getUserObject()).getContainingMolecules().get(j).getCardinality();
						}
					}
				}
				if(stoich>1){				
					eData.setStoichiometry(stoich);			
					e = new BiochamEdge(eData);				
					BiochamGraphConstants.setLabelAlongEdge(e.getAttributes(), true);
					BiochamGraphConstants.setLabelEnabled(e.getAttributes(),true);
					BiochamGraphConstants.setExtraLabels(e.getAttributes(),new Object[]{new JLabel(Integer.toString(eData.getStoichiometry()))});
					molecules.add(((BiochamEntityData)targets.get(i).getInstance().getUserObject()).getId());	
					eData.setMolecules(molecules);
					edge.setUserObject(eData);
				}else{
					 e = new DefaultEdge();
					 BiochamGraphConstants.setSelectable(e.getAttributes(),true);
					 BiochamGraphConstants.setLineStyle(e.getAttributes(), BiochamGraphConstants.STYLE_ORTHOGONAL);
					 BiochamGraphConstants.setBendable(e.getAttributes(), false);
				}
				GraphConstants.setLineEnd(e.getAttributes(), BiochamGraphConstants.ARROW_CLASSIC);
				GraphConstants.setEndFill(e.getAttributes(), true);
				if(opposite){
					e.setSource(((MiddleEdgeDissociation)mEdge).getLEFTIntermediateVertex().addPort());
				}else{
					e.setSource(((MiddleEdgeDissociation)mEdge).getRIGHTIntermediateVertex().addPort());
				}				
				e.setTarget(targets.get(i).getPort());		
				p=((BiochamEntityData)targets.get(i).getInstance().getUserObject()).getNumberOfConnections();
				p+=1;
				((BiochamEntityData)targets.get(i).getInstance().getUserObject()).setNumberOfConnections(p);
				((BiochamEntityData)targets.get(i).getInstance().getUserObject()).getInvolvedInReactions().add(rule);
				if(p>1){
					((BiochamEntityData)targets.get(i).getInstance().getUserObject()).setResponsable(true);
				}
				comps.add(targets.get(i).getInstance());
				children[2+i] = e;	
				comps.add(e);
			}
		}
		ArrayList<BiochamObject> ar=new ArrayList<BiochamObject>(1);
		ar.add(source);
		
		double cX=this.intermediateVertexMIDDLE.getX();
		double cY=this.intermediateVertexMIDDLE.getY();
		/*if(cX<60){
			cX=80;
			this.intermediateVertexMIDDLE.setBounds(cX,cY);		
		}*/
		if(!opposite){
			if(targets.size()==1){
				GraphUtilities.setSourcesLocation(graph,ar,cX-20,cY+7);
				GraphUtilities.setTargetsLocation(graph,targets,cX+40,cY+5);
			}else{
				GraphUtilities.setSourcesLocation(graph,ar,cX-20,cY+8);
				GraphUtilities.setTargetsLocation(graph,targets,cX+30,cY-10*targets.size());
			}
		}
		BiochamGraphConstants.setEditable(this.getAttributes(),true);
		BiochamGraphConstants.setDisconnectable(this.getAttributes(),false);
		BiochamGraphConstants.setMoveable(this.getAttributes(),true);
		BiochamGraphConstants.setSelectable(this.getAttributes(),true);
		BiochamGraphConstants.setChildrenSelectable(this.getAttributes(),true);
		graphic.getGraphLayoutCache().insertGroup(this,children);		
		
		graphic.getContainingRules().put(rule.trim(),comps.toArray());
		graph=graphic;
		comps.clear();
		comps=null;
		GraphUtilities.setLineStyleOrthogonalSelectedCells(graph);
		//ArrayList<BiochamObject> as=new ArrayList<BiochamObject>();
		//as.add(source);
		//userObject=GraphUtilities.setReactants(userObject,ar);
		//userObject=GraphUtilities.setProducts(userObject,targets);
		//userObject=GraphUtilities.setModulators2(userObject,modulators);
		userObject.setName(id);
		this.setUserObject(userObject);
		cell=userObject;
		super.setUserObject(userObject);
		containingMolecules=userObject.getMolecules();		
		//graphic.updateReaction(rule,null);
		ar=null;
		
		if(!opposite){
			((MiddleEdgeDissociation)getMiddleEdge()).setHorizontal();
		}
	}
	

	


	public BiochamGraph getGraph(){
		return graph;
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
		return new MiddleEdgeDissociation(cell,graphic,x,y,isOpposite());
	}
	
	public IntermediateVertex2 getLEFTIntermediateVertex() {
		return intermediateVertexLEFT;
	}
	public IntermediateVertexDissoc getMIDDLEIntermediateVertex() {
		return intermediateVertexMIDDLE;
	}
	public IntermediateVertex3 getRIGHTIntermediateVertex() {
		return intermediateVertexRIGHT;
	}	
	public String toString(){
		return "";
	}
	public void addReactant() {
		JOptionPane.showMessageDialog(graph.getBiochamModel().getGraphEditor(),"You can't add a reactant to a Dissociation.","Warning",JOptionPane.WARNING_MESSAGE);		
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
					 BiochamGraphConstants.setBendable(edge.getAttributes(), true);
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
			
		//	dissocConnection.addPort();
			//int len=dissocConnection.getChildCount();
			int p=((BiochamEntityData)o.getInstance().getUserObject()).getNumberOfConnections();
			p+=1;
			((BiochamEntityData)o.getInstance().getUserObject()).setNumberOfConnections(p);
			((BiochamEntityData)o.getInstance().getUserObject()).getInvolvedInReactions().add(rule);
			if(p>1){
				((BiochamEntityData)o.getInstance().getUserObject()).setResponsable(true);
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
			
			if(opposite){
				getLEFTIntermediateVertex().addPort();
				int len=getLEFTIntermediateVertex().getChildCount();
				edge.setSource(getLEFTIntermediateVertex().getChildAt(len-1));
			}else{
				getRIGHTIntermediateVertex().addPort();
				int len=getRIGHTIntermediateVertex().getChildCount();
				edge.setSource(getRIGHTIntermediateVertex().getChildAt(len-1));
			}
			
			
			edge.setTarget(o.getPort());
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


	public void addModulator(String nm1, int stoich, boolean fromChange) {
		
		String rule=this.getName();		
		Object[] newObjects=new Object[2];
		DefaultGraphCell cell=GraphUtilities.getCellByName(graph,nm1);
		((BiochamEntityData)cell.getUserObject()).setModulator(true);	
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
		modulators.add(cell);	
		if(!userObject.getModulators1AsStrings().contains(((BiochamEntityData)cell.getUserObject()).getName())){
			userObject.getModulators1().add(new BiochamEnitity(((BiochamEntityData)cell.getUserObject()).getId(),((BiochamEntityData)cell.getUserObject()).getName()));
		}
		//userObject.getModulators1().add(new BiochamEnitity(((BiochamEntityData)cell.getUserObject()).getId(),((BiochamEntityData)cell.getUserObject()).getName()));
		userObject.getMolecules().add(((BiochamEntityData)cell.getUserObject()).getId());
    	this.containingMolecules=userObject.getMolecules();
    	
		getMIDDLEIntermediateVertex().addPort();
		int len=getMIDDLEIntermediateVertex().getChildCount();		
		int p=((BiochamEntityData)o.getInstance().getUserObject()).getNumberOfConnections();
		p+=1;
		((BiochamEntityData)o.getInstance().getUserObject()).setNumberOfConnections(p);
		((BiochamEntityData)o.getInstance().getUserObject()).getInvolvedInReactions().add(rule);
		if(p>1){
			((BiochamEntityData)o.getInstance().getUserObject()).setResponsable(true);
		}
		edge.setSource(o.getPort());
		edge.setTarget(getMIDDLEIntermediateVertex().getChildAt(len-1));
		
		BiochamGraphConstants.setLabelEnabled(edge.getAttributes(),true);
		BiochamGraphConstants.setLabelAlongEdge(edge.getAttributes(), true);
		BiochamGraphConstants.setExtraLabels(edge.getAttributes(),new Object[]{new JLabel(Integer.toString(stoich))});
		BiochamGraphConstants.setLineEnd(edge.getAttributes(),BiochamGraphConstants.ARROW_DIAMOND);//.ARROW_INHIBITION);//.ARROW_CIRCLE
		BiochamGraphConstants.setEndFill(edge.getAttributes(),false);			
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
		
		cell=null;
		edge=null;		
		o=null;
		eData=null;
		molecules=null;
		rule=null;
		newObjects=null;
	}

	
	public String getName() {		
		return id;
	}


	public String getId() {
		return id;
	}


	public void setId(String id) {
		this.id = id;
	}


	public ArrayList<UUID> getContainingMolecules() {
		return containingMolecules;
	}


	public void setContainingMolecules(ArrayList<UUID> containingMolecules) {
		this.containingMolecules = containingMolecules;
	}

	public Color getColor() {
		return color;
	}
	
	public void setColor(Color blue) {
	
		this.color=blue;
		 Map nested = new Hashtable();     
		 for(int i=0;i<graph.getContainingRules().get(this.getId()).length;i++){
			 if(graph.getContainingRules().get(this.getId())[i] instanceof MiddleEdgeDissociation){
				 MiddleEdgeDissociation iv=(MiddleEdgeDissociation)graph.getContainingRules().get(this.getId())[i];
					iv.setColor(blue);
			 }else{
				 Map attributeMap1 = new Hashtable();
				 BiochamGraphConstants.setLineColor(attributeMap1, blue);
				 BiochamGraphConstants.setBendable(attributeMap1,false);
				 if(graph.getContainingRules().get(this.getId())[i]!=null){
					 nested.put(graph.getContainingRules().get(this.getId())[i], attributeMap1);
				 }
			 }
		 }	
		 graph.getGraphLayoutCache().edit(nested, null, null, null);
		 BiochamGraphConstants.setLineColor(this.getAttributes(), blue);
		
	}

	public Location getNextModulatorLocation() {
		
		nextModulatorLocation=calculateNewModulatorLocation();		
		return nextModulatorLocation;
	}

	public void setNextModulatorLocation(Location nextModulatorLocation) {
		this.nextModulatorLocation = nextModulatorLocation;
	}
	
	public Location getNextTargetLocation() {
		nextTargetLocation=calculateNewTargetLocation();
		return nextTargetLocation;
	}

	public void setNextTargetLocation(Location nextTargetLocation) {
		this.nextTargetLocation = nextTargetLocation;
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

	public void setPosition(double x, double y){
		
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();		
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(x,y,getMIDDLEIntermediateVertex().WIDTH,getMIDDLEIntermediateVertex().HEIGHT));
		nested.put(this.getMIDDLEIntermediateVertex(), attributeMap1);	
		getMIDDLEIntermediateVertex().setX(x);
		getMIDDLEIntermediateVertex().setY(y);
		
		this.getMIDDLEIntermediateVertex().setNewX(x);
		this.getMIDDLEIntermediateVertex().setNewY(y);
		
		attributeMap1 = new Hashtable();	
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(x+30,y+5,getRIGHTIntermediateVertex().WIDTH,getRIGHTIntermediateVertex().HEIGHT));
		nested.put(getRIGHTIntermediateVertex(), attributeMap1);
		getRIGHTIntermediateVertex().setX(x+30);
		getRIGHTIntermediateVertex().setY(y+5);
		
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested);
		}
		graph.refresh();
	}
	
	public void setPosition(double x, double y, boolean begin){
		
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();		
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(x,y,getMIDDLEIntermediateVertex().WIDTH,getMIDDLEIntermediateVertex().HEIGHT));
		nested.put(this.getMIDDLEIntermediateVertex(), attributeMap1);	
		getMIDDLEIntermediateVertex().setX(x);
		getMIDDLEIntermediateVertex().setY(y);
		
		this.getMIDDLEIntermediateVertex().setNewX(x);
		this.getMIDDLEIntermediateVertex().setNewY(y);
		
		attributeMap1 = new Hashtable();	
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(x+30,y+5,getRIGHTIntermediateVertex().WIDTH,getRIGHTIntermediateVertex().HEIGHT));
		nested.put(getRIGHTIntermediateVertex(), attributeMap1);
		getRIGHTIntermediateVertex().setX(x+30);
		getRIGHTIntermediateVertex().setY(y+5);
		
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested);
		}
		if(begin){
			ArrayList<BiochamObject> ar=new ArrayList<BiochamObject>(1);
			ar.add(source);
			GraphUtilities.setSourcesLocation(graph,ar,x-20,y+8);
			ar=null;
		}		
		
		GraphUtilities.setTargetsLocation(graph,targets,x+30,y-45);
		
		
		
		
	}
	
}
