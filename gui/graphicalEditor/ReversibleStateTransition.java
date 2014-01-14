package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.GraphConstants;

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





public class ReversibleStateTransition extends DefaultGraphCell {

	private DefaultGraphCell[] children;
	private BiochamGraph graph;
	IntermediateVertex intermediateVertexMIDDLE1,intermediateVertexMIDDLE2;
	IntermediateVertex2 intermediateVertexLEFT1,intermediateVertexLEFT2;
	IntermediateVertex3 intermediateVertexRIGHT1,intermediateVertexRIGHT2;
	ArrayList<BiochamObject> sources, targets;
	ArrayList<DefaultGraphCell> modulators1,modulators2;
	BiochamEdgeData userObject;
	DefaultGraphCell modulator;	
	StateTransition reversibleStateTransition;
	DefaultGraphCell middleEdge1,middleEdge2;
	String id;
	Object source,target;
	ArrayList<UUID> containingMolecules;
	String leftSide,rightSide;
	Color color;
	Location nextSourceLocation, nextTargetLocation, nextModulator1Location,nextModulator2Location;
	
	
	
	
	public ReversibleStateTransition(Object cell,ArrayList<BiochamObject> sources, ArrayList<BiochamObject> targets,DefaultGraphCell modulator,DefaultGraphCell reversModulator,BiochamGraph graphic,boolean reversibleSide, String rule){		
		super(cell);	 
		createTransition(cell, sources, targets, modulator, reversModulator, graphic, reversibleSide, rule);	
	}
	
	public ReversibleStateTransition(Object cell, ArrayList<DefaultGraphCell> reacts, ArrayList<DefaultGraphCell> prods, Object object, Object object2, BiochamGraph graph, boolean reversibleSide, String rule) {
		
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
	
	
	private void createTransition(Object cell, ArrayList<BiochamObject> sources, ArrayList<BiochamObject> targets, DefaultGraphCell modulator, DefaultGraphCell reversModulator, BiochamGraph graphic, boolean reversibleSide, String rule) {
		
	
		containingMolecules=new ArrayList<UUID>();
		userObject=(BiochamEdgeData) cell;		
		ArrayList comps=new ArrayList();
		setId(rule);
	
		graph=graphic;
		this.sources=new ArrayList<BiochamObject>(sources);
		this.targets=new ArrayList<BiochamObject>(targets);			
		children=new DefaultGraphCell[2+2*sources.size()+2*targets.size()];		
				
		DefaultGraphCell mEdge1, mEdge2;
		userObject.setReversibilityType(ReversibilityType.DOUBLE);
		BiochamEdgeData dt1=new BiochamEdgeData(userObject);
		mEdge1=createMiddleEdge(dt1,graphic);		
		this.intermediateVertexLEFT1=((MiddleEdge)mEdge1).getLEFTIntermediateVertex();
		this.intermediateVertexMIDDLE1=((MiddleEdge)mEdge1).getMIDDLEIntermediateVertex();
		this.intermediateVertexRIGHT1=((MiddleEdge)mEdge1).getRIGHTIntermediateVertex();		
		children[0]=mEdge1;
		setMiddleEdge1(mEdge1);
		BiochamEdgeData dt2=new BiochamEdgeData(userObject);
		mEdge2=createMiddleEdge(dt2,graphic,this.intermediateVertexMIDDLE1.getX(),this.intermediateVertexMIDDLE1.getY()-25);
		BiochamGraphConstants.setRouting(mEdge1.getAttributes(), MyParallelEdgeRouter.getSharedInstance());
		BiochamGraphConstants.setRouting(mEdge2.getAttributes(), MyParallelEdgeRouter.getSharedInstance());	
		
		this.intermediateVertexLEFT2=((MiddleEdge)mEdge2).getLEFTIntermediateVertex();
		this.intermediateVertexMIDDLE2=((MiddleEdge)mEdge2).getMIDDLEIntermediateVertex();
		this.intermediateVertexRIGHT2=((MiddleEdge)mEdge2).getRIGHTIntermediateVertex();		
		children[1]=mEdge2;
		setMiddleEdge2(mEdge2);
		comps.add(mEdge1);
		comps.add(mEdge2);		
		int len=5;
		if(len>10){
			this.intermediateVertexLEFT1.setDistance(20+len);
			this.intermediateVertexLEFT2.setDistance(20+len);
			this.intermediateVertexRIGHT1.setDistance(20+len);
			this.intermediateVertexRIGHT2.setDistance(20+len);
		}
		for(int i=0;i<sources.size();i++){
			
			DefaultEdge edge1=null,edge2;							
			ArrayList<UUID> molecules=new ArrayList<UUID>();
			BiochamEdgeData eData=new BiochamEdgeData(graph,molecules);			
			if(sources.get(i).getStoichiometry()>1){
						
				eData.setStoichiometry(sources.get(i).getStoichiometry());				
				edge1 = new BiochamEdge(eData);
				edge2 = new BiochamEdge(eData);	
				BiochamGraphConstants.setLabelEnabled(edge1.getAttributes(),true);
				BiochamGraphConstants.setLabelAlongEdge(edge1.getAttributes(), true);
				BiochamGraphConstants.setExtraLabels(edge1.getAttributes(),new Object[]{new JLabel(Integer.toString(eData.getStoichiometry()))});
				BiochamGraphConstants.setLabelEnabled(edge2.getAttributes(),true);
				BiochamGraphConstants.setLabelAlongEdge(edge2.getAttributes(), true);
				BiochamGraphConstants.setExtraLabels(edge2.getAttributes(),new Object[]{new JLabel(Integer.toString(eData.getStoichiometry()))});
				molecules.add(((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getId());
				eData.setMolecules(molecules);
				edge1.setUserObject(eData);
				edge2.setUserObject(eData);
			}else{				
				edge1 = new DefaultEdge();
				BiochamGraphConstants.setRouting(edge1.getAttributes(), MyParallelEdgeRouter.getSharedInstance());
				BiochamGraphConstants.setSelectable(edge1.getAttributes(),true);
				BiochamGraphConstants.setLineStyle(edge1.getAttributes(), BiochamGraphConstants.STYLE_ORTHOGONAL);
				BiochamGraphConstants.setBendable(edge1.getAttributes(), false);	
				edge2 = new DefaultEdge();
				BiochamGraphConstants.setSelectable(edge2.getAttributes(),true);
				BiochamGraphConstants.setLineStyle(edge2.getAttributes(), BiochamGraphConstants.STYLE_ORTHOGONAL);
				BiochamGraphConstants.setBendable(edge2.getAttributes(), false);	
				BiochamGraphConstants.setRouting(edge2.getAttributes(), MyParallelEdgeRouter.getSharedInstance());
			}
			
			
			int p=((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getNumberOfConnections();
			p+=1;
			((BiochamEntityData)sources.get(i).getInstance().getUserObject()).setNumberOfConnections(p);
			((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getInvolvedInReactions().add(rule);
			((BiochamEntityData)targets.get(i).getInstance().getUserObject()).getInvolvedInReactions().add(rule);
			if(p>1){
				((BiochamEntityData)sources.get(i).getInstance().getUserObject()).setResponsable(true);
			}
			
			edge1.setSource(sources.get(i).getPort());					
			edge1.setTarget(((MiddleEdge)mEdge1).getLEFTIntermediateVertex().addPort());
			edge2.setTarget(sources.get(i).getInstance().addPort());					
			edge2.setSource(((MiddleEdge)mEdge2).getLEFTIntermediateVertex().addPort());	
			BiochamGraphConstants.setLineEnd(edge2.getAttributes(), BiochamGraphConstants.ARROW_CLASSIC);
			BiochamGraphConstants.setEndFill(edge2.getAttributes(), true);
			children[2*i+2] = edge1;
			children[2*i+2+1] = edge2;
			comps.add(sources.get(i).getInstance());
			comps.add(edge1);
			comps.add(edge2);
		}
		
		
		
		for(int i=0;i<targets.size();i++){
			
			DefaultEdge edge3,edge4;
			BiochamEdgeData eData=null;
			ArrayList<UUID> molecules=new ArrayList<UUID>();	
			eData=new BiochamEdgeData(graph,molecules);
			if(targets.get(i).getStoichiometry()>1){
				
				eData.setStoichiometry(targets.get(i).getStoichiometry());				
				edge3 = new BiochamEdge(eData);				
				BiochamGraphConstants.setLabelEnabled(edge3.getAttributes(),true);
				BiochamGraphConstants.setLabelAlongEdge(edge3.getAttributes(), true);				
				BiochamGraphConstants.setExtraLabels(edge3.getAttributes(),new Object[]{new JLabel(Integer.toString(eData.getStoichiometry()))});
				molecules.add(((BiochamEntityData)targets.get(i).getInstance().getUserObject()).getId());
				eData.setMolecules(molecules);
				edge3.setUserObject(eData);
				edge4 = new BiochamEdge(eData);				
				BiochamGraphConstants.setLabelEnabled(edge4.getAttributes(),true);
				BiochamGraphConstants.setLabelAlongEdge(edge4.getAttributes(), true);				
				BiochamGraphConstants.setExtraLabels(edge4.getAttributes(),new Object[]{new JLabel(Integer.toString(eData.getStoichiometry()))});
				molecules.add(((BiochamEntityData)targets.get(i).getInstance().getUserObject()).getId());
				eData.setMolecules(molecules);
				edge4.setUserObject(eData);
			}else{				
				edge3 = new DefaultEdge();	
				BiochamGraphConstants.setRouting(edge3.getAttributes(), MyParallelEdgeRouter.getSharedInstance());
				BiochamGraphConstants.setSelectable(edge3.getAttributes(),true);
				BiochamGraphConstants.setLineStyle(edge3.getAttributes(), BiochamGraphConstants.STYLE_ORTHOGONAL);
				BiochamGraphConstants.setBendable(edge3.getAttributes(), false);
				edge4 = new DefaultEdge();	
				BiochamGraphConstants.setSelectable(edge4.getAttributes(),true);
				BiochamGraphConstants.setLineStyle(edge4.getAttributes(), BiochamGraphConstants.STYLE_ORTHOGONAL);
				BiochamGraphConstants.setBendable(edge4.getAttributes(), false);
				BiochamGraphConstants.setRouting(edge4.getAttributes(), MyParallelEdgeRouter.getSharedInstance());
				
			}
			
			edge3.setSource(((MiddleEdge)mEdge1).getRIGHTIntermediateVertex().addPort());
			edge3.setTarget(targets.get(i).getPort());
			BiochamGraphConstants.setLineEnd(edge3.getAttributes(), BiochamGraphConstants.ARROW_CLASSIC);
			BiochamGraphConstants.setEndFill(edge3.getAttributes(), true);
			edge4.setTarget(((MiddleEdge)mEdge2).getRIGHTIntermediateVertex().addPort());
			edge4.setSource(targets.get(i).getInstance().addPort());
			
			
			int p=((BiochamEntityData)targets.get(i).getInstance().getUserObject()).getNumberOfConnections();
			p+=1;
			((BiochamEntityData)targets.get(i).getInstance().getUserObject()).setNumberOfConnections(p);
			((BiochamEntityData)targets.get(i).getInstance().getUserObject()).getInvolvedInReactions().add(rule);
			((BiochamEntityData)targets.get(i).getInstance().getUserObject()).getInvolvedInReactions().add(rule);
			if(p>1){
				((BiochamEntityData)targets.get(i).getInstance().getUserObject()).setResponsable(true);
			}					
			comps.add(targets.get(i).getInstance());
			comps.add(edge3);
			comps.add(edge4);
			children[sources.size()*2+2*i+2] = edge3;
			children[sources.size()*2+2*i+2+1] = edge4;					
		}			
		
		
		GraphUtilities.setSourcesLocation(graph,sources,intermediateVertexLEFT1.getX()-20,intermediateVertexLEFT1.getY());
		GraphUtilities.setTargetsLocation(graph,targets,intermediateVertexRIGHT1.getX(),intermediateVertexRIGHT1.getY());		
		BiochamGraphConstants.setRouting(this.getAttributes(), MyParallelEdgeRouter.getSharedInstance());
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
		this.setId(rule);
		graph=graphic;
		comps.clear();
		comps=null;
		GraphUtilities.setLineStyleOrthogonalSelectedCells(graph);
		userObject.setName(id);
		this.setUserObject(userObject);
		cell=userObject;
		super.setUserObject(userObject);
		containingMolecules=userObject.getMolecules();
		graphic.updateReaction(rule, null);				
		GraphUtilities.setLineStyleOrthogonalSelectedCells(graph);
	
	}
	private DefaultGraphCell createMiddleEdge(Object cell,BiochamGraph graphic, double x, double y) {
		
		return new MiddleEdge(cell,graphic,x,y);
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
	public DefaultGraphCell getMiddleEdge1() {
		return middleEdge1;
	}
	public void setMiddleEdge1(DefaultGraphCell middleEdge) {
		this.middleEdge1 = middleEdge;		
	}
	public DefaultGraphCell getMiddleEdge2() {
		return middleEdge2;
	}
	public void setMiddleEdge2(DefaultGraphCell middleEdge) {
		this.middleEdge2 = middleEdge;		
	}
	
	public void setVerticalUp(){
		this.getIntermediateVertexMIDDLE1().setBounds(this.getIntermediateVertexMIDDLE2().getX()-30,this.getIntermediateVertexMIDDLE2().getY());
		this.getIntermediateVertexLEFT1().setBounds(intermediateVertexMIDDLE1.getX()+intermediateVertexMIDDLE1.WIDTH/2,intermediateVertexMIDDLE1.getY()-10);
		this.getIntermediateVertexLEFT2().setBounds(intermediateVertexMIDDLE2.getX()+intermediateVertexMIDDLE2.WIDTH/2,intermediateVertexMIDDLE2.getY()-10);
		this.getIntermediateVertexRIGHT1().setBounds(intermediateVertexMIDDLE1.getX()+intermediateVertexMIDDLE1.WIDTH/2,intermediateVertexMIDDLE1.getY()+25);
		this.getIntermediateVertexRIGHT2().setBounds(intermediateVertexMIDDLE2.getX()+intermediateVertexMIDDLE2.WIDTH/2,intermediateVertexMIDDLE2.getY()+25);		
	}
	public void setVerticalDown(){
				
		this.getIntermediateVertexMIDDLE1().setBounds(this.getIntermediateVertexMIDDLE2().getX()+30,this.getIntermediateVertexMIDDLE2().getY());
		this.getIntermediateVertexLEFT1().setBounds(intermediateVertexMIDDLE1.getX()+intermediateVertexMIDDLE1.WIDTH/2,intermediateVertexMIDDLE1.getY()+25);
		this.getIntermediateVertexLEFT2().setBounds(intermediateVertexMIDDLE2.getX()+intermediateVertexMIDDLE2.WIDTH/2,intermediateVertexMIDDLE2.getY()+25);
		this.getIntermediateVertexRIGHT1().setBounds(intermediateVertexMIDDLE1.getX()+intermediateVertexMIDDLE1.WIDTH/2,intermediateVertexMIDDLE1.getY()-10);
		this.getIntermediateVertexRIGHT2().setBounds(intermediateVertexMIDDLE2.getX()+intermediateVertexMIDDLE2.WIDTH/2,intermediateVertexMIDDLE2.getY()-10);		
		
		
		/*this.getIntermediateVertexMIDDLE1().setBounds(this.getIntermediateVertexMIDDLE2().getX()-30,this.getIntermediateVertexMIDDLE2().getY());
		this.getIntermediateVertexLEFT1().setBounds(intermediateVertexMIDDLE1.getX()+intermediateVertexMIDDLE1.WIDTH/2,intermediateVertexMIDDLE1.getY()-10);
		this.getIntermediateVertexLEFT2().setBounds(intermediateVertexMIDDLE2.getX()+intermediateVertexMIDDLE2.WIDTH/2,intermediateVertexMIDDLE2.getY()-10);
		this.getIntermediateVertexRIGHT1().setBounds(intermediateVertexMIDDLE1.getX()+intermediateVertexMIDDLE1.WIDTH/2,intermediateVertexMIDDLE1.getY()+25);
		this.getIntermediateVertexRIGHT2().setBounds(intermediateVertexMIDDLE2.getX()+intermediateVertexMIDDLE2.WIDTH/2,intermediateVertexMIDDLE2.getY()+25);*/
		
		
	}
	public void setHorizontal(){
		this.getIntermediateVertexMIDDLE1().setBounds(this.getIntermediateVertexMIDDLE2().getX(),this.getIntermediateVertexMIDDLE2().getY()+30);
		this.getIntermediateVertexLEFT1().setBounds(intermediateVertexMIDDLE1.getX()-10,intermediateVertexMIDDLE1.getY()+intermediateVertexMIDDLE1.WIDTH/2);
		this.getIntermediateVertexLEFT2().setBounds(intermediateVertexMIDDLE2.getX()-10,intermediateVertexMIDDLE2.getY()+intermediateVertexMIDDLE2.WIDTH/2);
		this.getIntermediateVertexRIGHT1().setBounds(intermediateVertexMIDDLE1.getX()+25,intermediateVertexMIDDLE1.getY()+intermediateVertexMIDDLE1.WIDTH/2);
		this.getIntermediateVertexRIGHT2().setBounds(intermediateVertexMIDDLE2.getX()+25,intermediateVertexMIDDLE2.getY()+intermediateVertexMIDDLE2.WIDTH/2);
		
	}
	
	public void setHorizontalReverse(){
		((MiddleEdge)this.getMiddleEdge1()).setHorizontalReverse();
		((MiddleEdge)this.getMiddleEdge2()).setHorizontalReverse();
		this.getIntermediateVertexMIDDLE2().setBounds(this.getIntermediateVertexMIDDLE2().getX(),this.getIntermediateVertexMIDDLE2().getY()-5);
	}

	
	public String getId() {
		return id;
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
		    
		    ((MiddleEdge)this.getMiddleEdge1()).setColor(green);
		    ((MiddleEdge)this.getMiddleEdge2()).setColor(green);
		    
		  
		
	}


	public Color getColor() {
		return color;
	}



	
	public void addReactant() {
		
		String rule=this.getId();	
		AddReactantProductDialog d=new AddReactantProductDialog("Choose Reactant", graph);
		Object[] newObjects=new Object[2];
		
		if(d.getChosenReactant()!=null && d.getChosenReactant()!=""){
		
			boolean reversibleDir=false;
			Object[] possibilities = {"forward reaction", "reverse reaction"};  					
			String s = (String)JOptionPane.showInputDialog(BiochamMainFrame.frame,
							" \nTo with reaction you want to add this reactant?\n\n",
							"Add Reactant",
							JOptionPane.PLAIN_MESSAGE,
							Icons.icons.get("question.png"),
							possibilities,
							"forward reaction");
			if(s!=null){
				if(s.equals("reverse reaction")){
					reversibleDir=true;
				}else{
					reversibleDir=false;
				}
//				CAN YOU OR NOT!!!!!!!!!!!!
				
				
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
				if(reversibleDir){
					this.getIntermediateVertexRIGHT2().addPort();
					int len=getIntermediateVertexRIGHT2().getChildCount();
					edge1.setTarget(getIntermediateVertexRIGHT2().getChildAt(len-1));
				}else{
					this.getIntermediateVertexLEFT1().addPort();
					int len=getIntermediateVertexLEFT1().getChildCount();
					edge1.setTarget(getIntermediateVertexLEFT1().getChildAt(len-1));
				}
				
				graph.getGraphLayoutCache().toFront(new Object[]{edge1,cell});			
				graph.getGraphLayoutCache().insert(edge1);
				graph.refresh();
				newObjects[0]=cell;
				newObjects[1]=edge1;
				sources.add(o);
				
				String newRule;
				String ruleToChange;
				String tmp;
				if(reversibleDir){
					ruleToChange=rule.substring(rule.indexOf(",")+1,rule.length()-1);
					tmp=GraphUtilities.addReactantToBiochamRule(rule.substring(rule.indexOf(",")+1,rule.length()-1),((BiochamEntityData)cell.getUserObject()).getName());
					newRule=rule.substring(0,rule.indexOf(",")+1)+tmp+"}";
				}else{
					ruleToChange=rule.substring(1,rule.indexOf(","));
					tmp=GraphUtilities.addReactantToBiochamRule(rule.substring(1,rule.indexOf(",")),((BiochamEntityData)cell.getUserObject()).getName());
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
		}
		d.dispose();
		d=null;
			
			
	}
	
	public void addProduct() {
		
		
		String rule=this.getId();					
		AddReactantProductDialog d=new AddReactantProductDialog("Choose Product", graph);			
		Object[] newObjects=new Object[2];
		
		if(d.getChosenReactant()!=null && d.getChosenReactant()!=""){
			
			boolean reversibleDir=false;
			Object[] possibilities = {"forward reaction", "reverse reaction"};  					
			String s = (String)JOptionPane.showInputDialog(BiochamMainFrame.frame,
							" \nTo with reaction you want to add this product?\n\n",
							"Add Product",
							JOptionPane.PLAIN_MESSAGE,
							Icons.icons.get("question.png"),
							possibilities,
							"forward reaction");
			if(s!=null){
				if(s.equals("reverse reaction")){
					reversibleDir=true;
				}else{
					reversibleDir=false;
				}
//				CAN YOU OR NOT!!!!!!!!!!!!
				
				
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
				
				if(reversibleDir){
					this.getIntermediateVertexLEFT2().addPort();
					int len=getIntermediateVertexLEFT2().getChildCount();
					edge.setSource(getIntermediateVertexLEFT2().getChildAt(len-1));
				}else{
					this.getIntermediateVertexRIGHT1().addPort();
					int len=getIntermediateVertexRIGHT1().getChildCount();
					edge.setSource(getIntermediateVertexRIGHT1().getChildAt(len-1));
				}
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
				targets.add(o);
				newObjects[0]=cell; 
				newObjects[1]=edge;
				
				String newRule;
				String ruleToChange;
				String tmp;
				if(reversibleDir){
					ruleToChange=rule.substring(rule.indexOf(",")+1,rule.length()-1);
					tmp=GraphUtilities.addProductToBiochamRule(rule.substring(rule.indexOf(",")+1,rule.length()-1),((BiochamEntityData)cell.getUserObject()).getName());
					newRule=rule.substring(0,rule.indexOf(",")+1)+tmp+"}";
				}else{
					ruleToChange=rule.substring(1,rule.indexOf(","));
					tmp=GraphUtilities.addProductToBiochamRule(rule.substring(1,rule.indexOf(",")),((BiochamEntityData)cell.getUserObject()).getName());
					newRule="{"+tmp+rule.substring(rule.indexOf(","));
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
		}	
		d.dispose();
		d=null;
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
			Location l=getNextModulator1Location();
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
			ruleToChange=rule.substring(1,rule.indexOf(","));
			tmp=GraphUtilities.addModulatorToBiochamRule(rule.substring(1,rule.indexOf(",")),((BiochamEntityData)cell.getUserObject()).getName());
			newRule="{"+tmp+rule.substring(rule.indexOf(","));
							
					
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
			Location l=getNextModulator2Location();
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
			ruleToChange=rule.substring(rule.indexOf(",")+1,rule.length()-1);
			tmp=GraphUtilities.addModulatorToBiochamRule(rule.substring(rule.indexOf(",")+1,rule.length()-1),((BiochamEntityData)cell.getUserObject()).getName());
			newRule=rule.substring(0,rule.indexOf(",")+1)+tmp+"}";
									
					
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




	public DefaultGraphCell[] getItsChildren() {
		return children;
	}




	public void setItsChildren(DefaultGraphCell[] children) {
		this.children = children;
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




	public IntermediateVertex getIntermediateVertexMIDDLE1() {
		return intermediateVertexMIDDLE1;
	}




	public void setIntermediateVertexMIDDLE1(
			IntermediateVertex intermediateVertexMIDDLE1) {
		this.intermediateVertexMIDDLE1 = intermediateVertexMIDDLE1;
	}




	public IntermediateVertex getIntermediateVertexMIDDLE2() {
		return intermediateVertexMIDDLE2;
	}




	public void setIntermediateVertexMIDDLE2(
			IntermediateVertex intermediateVertexMIDDLE2) {
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




	public ArrayList<DefaultGraphCell> getModulators1() {
		return modulators1;
	}




	public void setModulators1(ArrayList<DefaultGraphCell> modulators1) {
		this.modulators1 = modulators1;
	}




	public ArrayList<DefaultGraphCell> getModulators2() {
		return modulators2;
	}




	public void setModulators2(ArrayList<DefaultGraphCell> modulators2) {
		this.modulators2 = modulators2;
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




	public BiochamEdgeData getUserObject() {
		return userObject;
	}




	public void setUserObject(BiochamEdgeData userObject) {
		this.userObject = userObject;
	}




	public void setId(String id) {
		this.id = id;
	}
	
	
	public Location getNextModulator1Location() {
		
		nextModulator1Location=calculateNewModulator1Location();		
		return nextModulator1Location;
	}

	public void setNextModulato1rLocation(Location nextModulatorLocation) {
		this.nextModulator1Location = nextModulatorLocation;
	}
	
	public Location getNextModulator2Location() {
		
		nextModulator2Location=calculateNewModulator2Location();		
		return nextModulator2Location;
	}

	public void setNextModulato2rLocation(Location nextModulatorLocation) {
		this.nextModulator2Location = nextModulatorLocation;
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
	

	public void addModulator() {		
		
		AddReactantProductDialog d=new AddReactantProductDialog("Choose Modulator", graph);
		if(d.getChosenReactant()!=null && d.getChosenReactant()!=""){			
			
			String nm=d.getChosenReactant();
			int stoich=d.getStoichCoeff();	
			boolean reversibleDir=false;
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
			
			nm=null;
			stoich=0;
			d.dispose();
			d=null;
		}
		
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


	private Location calculateNewModulator1Location() {
				
		BiochamEntityData dt=(BiochamEntityData)modulators1.get(modulators1.size()-1).getUserObject();
		if(dt.getPosition()!=null){
			double x=dt.getPosition().getX()+GraphUtilities.getCellWidth(modulators1.get(modulators1.size()-1));
			double y=dt.getPosition().getY();		
			return new Location(x+10,y);
		}		
		return null;	
	}
	
	private Location calculateNewModulator2Location() {
		
		BiochamEntityData dt=(BiochamEntityData)modulators2.get(modulators2.size()-1).getUserObject();
		if(dt.getPosition()!=null){
			double x=dt.getPosition().getX()+GraphUtilities.getCellWidth(modulators2.get(modulators2.size()-1));
			double y=dt.getPosition().getY();		
			return new Location(x+10,y);
		}		
		return null;	
	}

	
}
