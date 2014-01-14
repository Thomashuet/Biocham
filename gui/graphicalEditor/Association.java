package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.GraphConstants;
import org.jgraph.util.Spline;

import fr.inria.contraintes.biocham.modelData.ParamTableRules;

import java.awt.Color;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import javax.swing.JLabel;
import javax.swing.JOptionPane;


public class Association extends BiochamReaction{
	
		
	
	
	
	private DefaultGraphCell[] children;
	private BiochamGraph graph;
	IntermediateVertexAssoc intermediateVertexMIDDLE;
	IntermediateVertex2 intermediateVertexLEFT;
	IntermediateVertex3 intermediateVertexRIGHT;	
	ArrayList<BiochamObject> sources;
	ArrayList<UUID> containingMolecules;
	ArrayList<DefaultGraphCell> modulators;
	BiochamEdgeData userObject;	
	BiochamObject target;
	DefaultGraphCell modulator;
	int reversible=0;
	String id;	
	String leftSide,rightSide;
	Color color;
	Location nextSourceLocation, nextModulatorLocation;
	DefaultGraphCell middleEdge;
	boolean opposite=false;
	
	
	/**
	 * 
	 * 
	 * Cell is UserObject
	 * Source is a Port, Vertex.getChildAt(x)
	 * Target is a Port, Vertex.getChildAt(y)
	 * 
	 * 
	 **/
	public Association(Object cell,ArrayList<BiochamObject> sources,BiochamObject target,DefaultGraphCell modulator,BiochamGraph graphic,String rule){				
		super(cell);		
		createAssociation(cell, sources, target, modulator, graphic, rule);	
	}

	/**
	 * 
	 * 
	 * Cell is UserObject
	 * Source is a Port, Vertex.getChildAt(x)
	 * Target is a Port, Vertex.getChildAt(y)
	 * 
	 * 
	 **/
	public Association(Object cell, ArrayList<DefaultGraphCell> reacts, DefaultGraphCell prods, DefaultGraphCell modulator,BiochamGraph graph, String rule) {
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

	
	
	
	
	/**
	 * 
	 * 
	 * Building the association.
	 * 
	 * 
	 **/
	private void createAssociation(Object cell, ArrayList<BiochamObject> sources, BiochamObject target, DefaultGraphCell modulator, BiochamGraph graphic, String rule) {
		
		
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
		userObject.setStoichiometry(target.getStoichiometry());
		ArrayList comps=new ArrayList();
		id=rule;
						
		this.sources=new ArrayList<BiochamObject>(sources);
		this.target=new BiochamObject(target.getInstance().addPort(),target.getStoichiometry(),((BiochamEntityData)target.getInstance().getUserObject()).getName(),target.getInstance());
		int size=2+sources.size();
		children=new DefaultGraphCell[size];
		
		
		DefaultGraphCell mEdge;
		mEdge=createMiddleEdge(cell,graphic);
		MiddleEdgeAssociation m=(MiddleEdgeAssociation)mEdge;		
		this.intermediateVertexMIDDLE=m.getMIDDLEIntermediateVertex();
		
		if(opposite){
			BiochamGraphConstants.setBounds(m.getMIDDLEIntermediateVertex().getAttributes(), new Rectangle2D.Double(pos.getX(),pos.getY()-20,m.getMIDDLEIntermediateVertex().WIDTH,m.getMIDDLEIntermediateVertex().HEIGHT));
			m.getMIDDLEIntermediateVertex().setX(pos.getX());
			m.getMIDDLEIntermediateVertex().setY(pos.getY()-20);
			BiochamGraphConstants.setBounds(m.getRIGHTIntermediateVertex().getAttributes(), new Rectangle2D.Double(pos.getX()+30,pos.getY()-20+5,m.getRIGHTIntermediateVertex().WIDTH,m.getRIGHTIntermediateVertex().HEIGHT));
			m.getRIGHTIntermediateVertex().setX(pos.getX()+30);
			m.getRIGHTIntermediateVertex().setY(pos.getY()-20+5);
						
			this.intermediateVertexMIDDLE=m.getMIDDLEIntermediateVertex();
			this.intermediateVertexRIGHT=m.getRIGHTIntermediateVertex();			
		}else{
			this.intermediateVertexLEFT=m.getLEFTIntermediateVertex();
		}
	
		children[0]=mEdge;	
		setMiddleEdge(mEdge);
				
		children[0].addPort();						
		comps.add(mEdge);
		// n -times.....
		for(int i=0;i<sources.size();i++){
			
		
			DefaultEdge edge;
			ArrayList<UUID> molecules=new ArrayList<UUID>();
			BiochamEdgeData eData=new BiochamEdgeData(graph,molecules);
			int stoich=((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getStoichoimetryIntegerForReaction();
			if(stoich>1){				
				eData.setStoichiometry(stoich);	
				edge = new BiochamEdge(eData);		
				BiochamGraphConstants.setSelectable(edge.getAttributes(),false);
				BiochamGraphConstants.setLabelAlongEdge(edge.getAttributes(), true);
				BiochamGraphConstants.setLabelEnabled(edge.getAttributes(),true);
				BiochamGraphConstants.setExtraLabels(edge.getAttributes(),new Object[]{new JLabel(Integer.toString(eData.getStoichiometry()))});
				molecules.add(((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getId());
				eData.setMolecules(molecules);
				edge.setUserObject(eData);
			}else{
				 edge = new DefaultEdge();
				 BiochamGraphConstants.setSelectable(edge.getAttributes(),true);
				 BiochamGraphConstants.setLineStyle(edge.getAttributes(), BiochamGraphConstants.STYLE_ORTHOGONAL);
				 BiochamGraphConstants.setBendable(edge.getAttributes(), false);
				 
			}
			int k=((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getNumberOfConnections();
			k+=1;
			((BiochamEntityData)sources.get(i).getInstance().getUserObject()).setNumberOfConnections(k);
			((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getInvolvedInReactions().add(rule);
			if(k>1){
				((BiochamEntityData)sources.get(i).getInstance().getUserObject()).setResponsable(true);
			}
			edge.setSource(sources.get(i).getPort());
			comps.add(sources.get(i).getInstance());
			if(opposite){
				edge.setTarget(((MiddleEdgeAssociation)mEdge).getRIGHTIntermediateVertex().addPort());
			}else{
				edge.setTarget(((MiddleEdgeAssociation)mEdge).getLEFTIntermediateVertex().addPort());
			}
				
		
			children[i+1] = edge;	
			comps.add(edge);
			
			molecules=null;
			eData=null;
			edge=null;
		}
		
	
		
		DefaultEdge edge1 = new DefaultEdge();
		
	
		 
		 BiochamGraphConstants.setSelectable(edge1.getAttributes(),true);
		edge1.setSource(((MiddleEdgeAssociation)mEdge).getMIDDLEIntermediateVertex().addPort());
		int k=((BiochamEntityData)target.getInstance().getUserObject()).getNumberOfConnections();
		k+=1;
		BiochamGraphConstants.setSelectable(edge1.getAttributes(),false);
		((BiochamEntityData)target.getInstance().getUserObject()).setNumberOfConnections(k);
		((BiochamEntityData)target.getInstance().getUserObject()).getInvolvedInReactions().add(rule);
		if(k>1){
			((BiochamEntityData)target.getInstance().getUserObject()).setResponsable(true);
		}
		edge1.setTarget(target.getInstance().addPort());	
		comps.add(target.getInstance());
		GraphConstants.setLineEnd(edge1.getAttributes(), BiochamGraphConstants.ARROW_CLASSIC);
		GraphConstants.setEndFill(edge1.getAttributes(), true);
		children[sources.size()+1] = edge1;
		comps.add(edge1);
		
		ArrayList<BiochamObject> ar=new ArrayList<BiochamObject>(1);
		ar.add(target);
		
		
		double cX=this.intermediateVertexMIDDLE.getX();
		double cY=this.intermediateVertexMIDDLE.getY();
		
		if(!opposite){
			if(sources.size()==1){
				GraphUtilities.setSourcesLocation(graph,sources,cX-30,cY+6);
				GraphUtilities.setTargetsLocation(graph,ar,cX,cY+5);
			}else{
				GraphUtilities.setSourcesLocation(graph,sources,cX-20,cY+10);
				GraphUtilities.setTargetsLocation(graph,ar,cX,cY);
			}
		}
		
		
		
		
		graphic.getGraphLayoutCache().insertGroup(this,children);
		
			
		BiochamGraphConstants.setEditable(this.getAttributes(),true);
		BiochamGraphConstants.setDisconnectable(this.getAttributes(),false);
		BiochamGraphConstants.setGroupOpaque(this.getAttributes(),true);
		BiochamGraphConstants.setMoveable(this.getAttributes(),true);
		BiochamGraphConstants.setSelectable(this.getAttributes(),true);		
		
		graphic.getContainingRules().put(rule.trim(),comps.toArray());
		
		graph=graphic;
		comps.clear();
		comps=null;
	
		userObject.setName(id);
		this.setUserObject(userObject);
		cell=userObject;
		super.setUserObject(userObject);
		
		containingMolecules=userObject.getMolecules();
		graphic.updateReaction(rule,null);
		ar=null;
		
		if(!opposite){
			((MiddleEdgeAssociation)getMiddleEdge()).setHorizontal();
		}

	}
	
	
	
	
	public void addReactant() {

		String rule=this.getName();
		Object[] newObjects=new Object[2];
		AddReactantProductDialog d=new AddReactantProductDialog("Choose Reactant", graph);		
				
		if(d.getChosenReactant()!=null && d.getChosenReactant()!=""){
		
			DefaultGraphCell cell=GraphUtilities.getCellByName(graph,d.getChosenReactant());
			DefaultEdge edge;
			
		
			int stoich=d.getStoichCoeff();			
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
			userObject.getReactants().add(new BiochamEnitity(((BiochamEntityData)cell.getUserObject()).getId(),((BiochamEntityData)cell.getUserObject()).getName()));
			this.setUserObject(userObject);
			super.setUserObject(userObject);
			containingMolecules=userObject.getMolecules();
			
			int p=((BiochamEntityData)cell.getUserObject()).getNumberOfConnections();
			p+=1;
			((BiochamEntityData)cell.getUserObject()).setNumberOfConnections(p);
			((BiochamEntityData)cell.getUserObject()).getInvolvedInReactions().add(id);
			if(p>1){
				((BiochamEntityData)cell.getUserObject()).setResponsable(true);
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
			
			sources.add(o);
			
			newObjects[0]=cell;
			newObjects[1]=edge;			
			
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
			
			cell=null;
			edge=null;
			newRule=null;
			nested = null;
			attributeMap1 = null;
			l=null;
			o=null;
			eData=null;
			molecules=null;
			d=null;
			newObjects=null;
			rule=null;
		}
		rule=null;
		newObjects=null;
		d.dispose();
		d=null;
	}


	public void addProduct() {
		JOptionPane.showMessageDialog(graph.getBiochamModel().getGraphEditor(),"You can't add a product to an Association.","Warning",JOptionPane.WARNING_MESSAGE);
		
	}

	
	public void addModulator() {

		AddReactantProductDialog d=new AddReactantProductDialog("Choose Modulator", graph);
		if(d.getChosenReactant()!=null && d.getChosenReactant()!=""){			
			addModulator(d.getChosenReactant(), d.getStoichCoeff(),true);
		}
		d.dispose();
		d=null;
		
	}

	/**
	 * @param nm1
	 * @param stoich
	 */
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
		int p=((BiochamEntityData)cell.getUserObject()).getNumberOfConnections();
		p+=1;
		((BiochamEntityData)cell.getUserObject()).setNumberOfConnections(p);
		((BiochamEntityData)cell.getUserObject()).getInvolvedInReactions().add(rule);
		if(p>1){
			((BiochamEntityData)cell.getUserObject()).setResponsable(true);
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

	
	
	
	
	
	
	
	
	

	public BiochamGraph getGraph(){
		return graph;
	}
	public IntermediateVertex2 getLEFTIntermediateVertex() {
		return intermediateVertexLEFT;
	}
	public IntermediateVertexAssoc getMIDDLEIntermediateVertex() {
		return intermediateVertexMIDDLE;
	}
	public IntermediateVertex3 getRIGHTIntermediateVertex() {
		return intermediateVertexRIGHT;
	}	
	public String toString(){
		return "";
	}
	/*public IntermediateVertexAssoc getAssocConnection() {
		return assocConnection;
	}
	public void setAssocConnection(IntermediateVertexAssoc assocConnection) {
		this.assocConnection = assocConnection;
	}*/
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

	public void setColor(Color blue) {
		
		this.color=blue;
		Map nested = new Hashtable();     
		MiddleEdgeAssociation iv;
		for(int i=0;i<graph.getContainingRules().get(this.getId()).length;i++){
			 if(graph.getContainingRules().get(this.getId())[i] instanceof MiddleEdgeAssociation){
					iv=(MiddleEdgeAssociation)graph.getContainingRules().get(this.getId())[i];
					iv.setColor(blue);
			 }else{
				 Map attributeMap1 = new Hashtable();
				 BiochamGraphConstants.setLineColor(attributeMap1, blue);
				 BiochamGraphConstants.setBendable(attributeMap1,false);
				 if(graph.getContainingRules().get(this.getId())[i]!=null){
					 nested.put(graph.getContainingRules().get(this.getId())[i], attributeMap1);	 
				 }				 
				 attributeMap1=null;				 
			 }
		}	
		graph.getGraphLayoutCache().edit(nested, null, null, null);
		BiochamGraphConstants.setLineColor(this.getAttributes(), blue);
		nested=null;
		iv=null;
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
		return new MiddleEdgeAssociation(cell,graphic,x,y,isOpposite());
	}
	
	
	private Location calculateNewSourceLocation() {		
		
		int size=sources.size();
		BiochamEntityData dt=(BiochamEntityData)sources.get(size-1).getInstance().getUserObject();
		if(dt.getPosition()!=null){
			double x=dt.getPosition().getX();
			double y=dt.getPosition().getY()+(GraphUtilities.calculateMaxHeight(sources))*(sources.size()-1);	
			dt=null;
			return new Location(x,y+10);
		}	
		dt=null;
		return null;	
	}
	

	private Location calculateNewModulatorLocation() {
				
		BiochamEntityData dt=(BiochamEntityData)modulators.get(modulators.size()-1).getUserObject();
		if(dt.getPosition()!=null){
			double x=dt.getPosition().getX()+GraphUtilities.getCellWidth(modulators.get(modulators.size()-1));
			double y=dt.getPosition().getY();
			dt=null;
			return new Location(x+10,y);
		}		
		dt=null;
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
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(x-30,y+5,getLEFTIntermediateVertex().WIDTH,getLEFTIntermediateVertex().HEIGHT));
		nested.put(getLEFTIntermediateVertex(), attributeMap1);
		getLEFTIntermediateVertex().setX(x-30);
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
		nested.put(this.getMIDDLEIntermediateVertex(), attributeMap1);	
		getMIDDLEIntermediateVertex().setX(x);
		getMIDDLEIntermediateVertex().setY(y);	
		
		this.getMIDDLEIntermediateVertex().setNewX(x);
		this.getMIDDLEIntermediateVertex().setNewY(y);
		attributeMap1 = new Hashtable();	
		BiochamGraphConstants.setBounds(attributeMap1, new Rectangle2D.Double(x-30,y+5,getLEFTIntermediateVertex().WIDTH,getLEFTIntermediateVertex().HEIGHT));
		nested.put(getLEFTIntermediateVertex(), attributeMap1);
		getLEFTIntermediateVertex().setX(x-30);
		getLEFTIntermediateVertex().setY(y+5);
		
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested);
		}
		if(begin){
			GraphUtilities.setSourcesLocation(graph,sources,x-10,y+10);
		}		
		ArrayList<BiochamObject> ar=new ArrayList<BiochamObject>(1);
		ar.add(target);
		GraphUtilities.setTargetsLocation(graph,ar,x,y);
		graph.refresh();
				
		ar=null;
	}
	
	
}
