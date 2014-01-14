package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Rectangle2D;
import java.util.Hashtable;
import java.util.Map;

public class ReversibleMiddleEdge extends DefaultGraphCell implements CustomEdge, ActionListener {

	
	IntermediateVertex2 intermediateVertexLEFT;
	IntermediateVertex intermediateVertexMIDDLE1,intermediateVertexMIDDLE2;
	IntermediateVertex3 intermediateVertexRIGHT;
	BiochamGraph graph;
	MiddleEdge me1, me2;
	Object[] comps;
	
	
	
	
	
	
	
	public ReversibleMiddleEdge(Object cell,BiochamGraph graph,double x, double y) {
		
		
		
		this.graph=graph;
		DefaultGraphCell[] children=new DefaultGraphCell[4];		
		MiddleEdge me1=new MiddleEdge(cell,graph,x,y);
		MiddleEdge me2=null;
		
		if(cell instanceof BiochamEdgeData){
			
			BiochamEdgeData dt=new BiochamEdgeData(((BiochamEdgeData)cell).getProperties(),((BiochamEdgeData)cell).getGraph(),((BiochamEdgeData)cell).getMolecules());
			dt.setPosition(new Position(me1.getMIDDLEIntermediateVertex().getX(),me1.getMIDDLEIntermediateVertex().getY()+30,0,0));
			me2=new MiddleEdge(dt,graph,me1.getMIDDLEIntermediateVertex().getX(),me1.getMIDDLEIntermediateVertex().getY()+30);
			dt=null;
		}else{
			me2=new MiddleEdge(cell,graph,me1.getMIDDLEIntermediateVertex().getX(),me1.getMIDDLEIntermediateVertex().getY()+30);
		}
		
		this.me1=me1;
		this.me2=me2;		
		intermediateVertexMIDDLE1=this.me1.getMIDDLEIntermediateVertex();	
		intermediateVertexMIDDLE2=this.me2.getMIDDLEIntermediateVertex();		
		intermediateVertexLEFT=me1.getLEFTIntermediateVertex();
		intermediateVertexRIGHT=me1.getRIGHTIntermediateVertex();		
		children[0]=me1;
		children[1]=me2;
		
		DefaultEdge edge1 = new DefaultEdge();
		edge1.setSource(me1.getRIGHTIntermediateVertex().addPort());
		edge1.setTarget(me2.getRIGHTIntermediateVertex().addPort());	
		children[2] = edge1;
		
		DefaultEdge edge2 = new DefaultEdge();
		edge2.setSource(me2.getLEFTIntermediateVertex().addPort());
		edge2.setTarget(me1.getLEFTIntermediateVertex().addPort());		
		BiochamGraphConstants.setLineEnd(edge2.getAttributes(), BiochamGraphConstants.ARROW_CLASSIC);
		BiochamGraphConstants.setEndFill(edge2.getAttributes(), true);
		children[3] = edge2;
		
		BiochamGraphConstants.setEditable(this.getAttributes(),false);
		BiochamGraphConstants.setDisconnectable(this.getAttributes(),false);
		BiochamGraphConstants.setGroupOpaque(this.getAttributes(),true);
		BiochamGraphConstants.setMoveable(this.getAttributes(),true);
		BiochamGraphConstants.setSelectable(this.getAttributes(),false);
		BiochamGraphConstants.setChildrenSelectable(this.getAttributes(),false);
				
		graph.getGraphLayoutCache().insertGroup(this,children);	
		
		this.setComps(children);
		
		edge1=null;
		edge2=null;		
		
	}
	
	
	
	
	
	
	
	
	public IntermediateVertex2 getLEFTIntermediateVertex() {
		return intermediateVertexLEFT;
	}
	public IntermediateVertex getMIDDLEIntermediateVertex() {
		return intermediateVertexMIDDLE1;
	}
	public IntermediateVertex3 getRIGHTIntermediateVertex() {
		return intermediateVertexRIGHT;
	}
	public IntermediateVertex getIntermediateVertexMIDDLE1() {
		return intermediateVertexMIDDLE1;
	}
	public void setIntermediateVertexMIDDLE1(IntermediateVertex im) {
		this.intermediateVertexMIDDLE1 = im;
	}
	public IntermediateVertex getIntermediateVertexMIDDLE2() {
		return intermediateVertexMIDDLE2;
	}
	public void setIntermediateVertexMIDDLE2(IntermediateVertex im) {
		this.intermediateVertexMIDDLE2 = im;
	}

	
	
	
	
	
	
	public void setVertical(){
			
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();
		BiochamGraphConstants.setBounds(attributeMap1,new Rectangle2D.Double(me2.getMIDDLEIntermediateVertex().getX()+20+IntermediateVertex.WIDTH,me1.getMIDDLEIntermediateVertex().getY(),12,12));
		me2.getMIDDLEIntermediateVertex().setX(me2.getMIDDLEIntermediateVertex().getX()+20+IntermediateVertex.WIDTH);
		me2.getMIDDLEIntermediateVertex().setY(me1.getMIDDLEIntermediateVertex().getY());
		nested.put(me2.getMIDDLEIntermediateVertex(), attributeMap1 );
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested, null, null, null);
		}
		BiochamEdgeData dt=(BiochamEdgeData)me2.getMIDDLEIntermediateVertex().getUserObject();
		dt.setPosition(new Position(me2.getMIDDLEIntermediateVertex().getX(),me2.getMIDDLEIntermediateVertex().getY(),0,0));
		me2.getLEFTIntermediateVertex().setVerticalDown(me2.getMIDDLEIntermediateVertex().getX(),me2.getMIDDLEIntermediateVertex().getY());
		me2.getRIGHTIntermediateVertex().setVerticalUp(me2.getMIDDLEIntermediateVertex().getX(),me2.getMIDDLEIntermediateVertex().getY());
		me1.getLEFTIntermediateVertex().setVerticalDown(me1.getMIDDLEIntermediateVertex().getX(),me1.getMIDDLEIntermediateVertex().getY());
		me1.getRIGHTIntermediateVertex().setVerticalUp(me1.getMIDDLEIntermediateVertex().getX(),me1.getMIDDLEIntermediateVertex().getY());
		
		dt=null;
		nested=null;
		attributeMap1=null;
		
	}
	
	public void setHorizontal(){
		
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();
		BiochamGraphConstants.setBounds(attributeMap1,new Rectangle2D.Double(me1.getMIDDLEIntermediateVertex().getX(),me1.getMIDDLEIntermediateVertex().getY()+20+IntermediateVertex.HEIGHT,12,12));
		me2.getMIDDLEIntermediateVertex().setX(me1.getMIDDLEIntermediateVertex().getX());
		me2.getMIDDLEIntermediateVertex().setY(me1.getMIDDLEIntermediateVertex().getY()+20+IntermediateVertex.HEIGHT);
		nested.put(me2.getMIDDLEIntermediateVertex(), attributeMap1 );
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested, null, null, null);
		}
		BiochamEdgeData dt=(BiochamEdgeData)me2.getMIDDLEIntermediateVertex().getUserObject();
		dt.setPosition(new Position(me2.getMIDDLEIntermediateVertex().getX(),me2.getMIDDLEIntermediateVertex().getY(),0,0));
		me1.getLEFTIntermediateVertex().setHorizontal(me1.getMIDDLEIntermediateVertex().getX(),me1.getMIDDLEIntermediateVertex().getY());
		me1.getRIGHTIntermediateVertex().setHorizontal(me1.getMIDDLEIntermediateVertex().getX(),me1.getMIDDLEIntermediateVertex().getY());
		me2.getLEFTIntermediateVertex().setHorizontal(me2.getMIDDLEIntermediateVertex().getX(),me2.getMIDDLEIntermediateVertex().getY());
		me2.getRIGHTIntermediateVertex().setHorizontal(me2.getMIDDLEIntermediateVertex().getX(),me2.getMIDDLEIntermediateVertex().getY());
		
		dt=null;
		nested=null;
		attributeMap1=null;
	}
	
	public BiochamGraph getGraph() {
		return graph;
	}

	public void setGraph(BiochamGraph graph) {
		this.graph = graph;
	}

	public void actionPerformed(ActionEvent e) {
		changeOrientationAction(e);
		
	}

	private void changeOrientationAction(ActionEvent e) {
		String cmd=e.getActionCommand();
		if(cmd.equals("verticalMiddleEdge")){		
			setVertical();		   
		}else if(cmd.equals("horizontalMiddleEdge")){
			setHorizontal();
		}else if(cmd.equals("edgeColor")){
			
		}
		
	}

	public String getName() {
		return "";
	}

	public void setColor(Color green) {
		
		Map nested = new Hashtable();  
		for(int i=0;i<getComps().length;i++){
			if(getComps()[i] instanceof MiddleEdge){
				MiddleEdge me=(MiddleEdge)getComps()[i];
				me.setColor(green);
				me=null;
			}else{
	    	 Map attributeMap1 = new Hashtable();
	    	 BiochamGraphConstants.setLineColor(attributeMap1, green);
	    	 BiochamGraphConstants.setBorderColor(attributeMap1, green);
	    	 BiochamGraphConstants.setBendable(attributeMap1,false);
	    	 nested.put(getComps()[i], attributeMap1);
	    	 attributeMap1=null;
			}
	    }
		graph.getGraphLayoutCache().edit(nested, null, null, null);
		nested=null;
	}

	public Object[] getComps() {
		return comps;
	}

	public void setComps(Object[] comps) {
		this.comps = comps;
	}




}
