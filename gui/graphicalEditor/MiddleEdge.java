package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;

import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Hashtable;
import java.util.Map;


public class MiddleEdge extends DefaultGraphCell implements CustomEdge{//, ActionListener{

	
	
	IntermediateVertex2 intermediateVertexLEFT;
	IntermediateVertex intermediateVertexMIDDLE;
	IntermediateVertex3 intermediateVertexRIGHT;
	BiochamGraph graph;
	Object[] comps;
	Location location;
	
	
	
	public MiddleEdge(Object cell,BiochamGraph graph,double x, double y) {
		

		this.graph=graph;
		DefaultGraphCell[] children=new DefaultGraphCell[5];	
				
		if(x!=0 && y!=0){
			intermediateVertexMIDDLE = new IntermediateVertex(graph,cell,x,y);
			intermediateVertexLEFT   = new IntermediateVertex2(graph,x,y);
			intermediateVertexRIGHT  = new IntermediateVertex3(graph,x,y);
		}else{
			intermediateVertexMIDDLE = new IntermediateVertex(graph,cell);
			intermediateVertexLEFT   = new IntermediateVertex2(intermediateVertexMIDDLE);
			intermediateVertexRIGHT  = new IntermediateVertex3(intermediateVertexMIDDLE);	
		}		
		
		
		children[0]=intermediateVertexLEFT;
		children[1]=intermediateVertexMIDDLE;
		children[2]=intermediateVertexRIGHT;
		
		
		DefaultEdge edge1 = new DefaultEdge();
		//BiochamGraphConstants.setRouting(edge1.getAttributes(), MyParallelEdgeRouter.getSharedInstance());
		edge1.setSource(children[0].addPort());
		edge1.setTarget(children[1].addPort());	
		children[3] = edge1;
		
		
		
		DefaultEdge edge2 = new DefaultEdge();
		//BiochamGraphConstants.setRouting(edge2.getAttributes(), MyParallelEdgeRouter.getSharedInstance());
		edge2.setSource(children[1].addPort());
		edge2.setTarget(children[2].addPort());	
		children[4] = edge2;
		
		BiochamGraphConstants.setEditable(this.getAttributes(),false);
		//BiochamGraphConstants.setLineStyle(edge1.getAttributes(),BiochamGraphConstants.STYLE_ORTHOGONAL);
		//BiochamGraphConstants.setLineStyle(edge2.getAttributes(),BiochamGraphConstants.STYLE_ORTHOGONAL);
		BiochamGraphConstants.setDisconnectable(this.getAttributes(),false);
		BiochamGraphConstants.setGroupOpaque(this.getAttributes(),true);
		BiochamGraphConstants.setMoveable(this.getAttributes(),true);
		BiochamGraphConstants.setSelectable(this.getAttributes(),true);
		BiochamGraphConstants.setChildrenSelectable(this.getAttributes(),false);
				
		graph.getGraphLayoutCache().insertGroup(this,children);	
		this.setComps(children);
		
	}
	
	

	public IntermediateVertex2 getLEFTIntermediateVertex() {
		return intermediateVertexLEFT;
	}
	public IntermediateVertex getMIDDLEIntermediateVertex() {
		return intermediateVertexMIDDLE;
	}
	public IntermediateVertex3 getRIGHTIntermediateVertex() {
		return intermediateVertexRIGHT;
	}

	public void setVerticalTB(){
		getLEFTIntermediateVertex().setVerticalDown(intermediateVertexMIDDLE.getX(),intermediateVertexMIDDLE.getY());
		getRIGHTIntermediateVertex().setVerticalUp(intermediateVertexMIDDLE.getX(),intermediateVertexMIDDLE.getY());	
		
	}
	
	public void setVerticalBT(){
		getLEFTIntermediateVertex().setVerticalUp(intermediateVertexMIDDLE.getX(),intermediateVertexMIDDLE.getY());
		getRIGHTIntermediateVertex().setVerticalDown(intermediateVertexMIDDLE.getX(),intermediateVertexMIDDLE.getY());	
		
	}
	
	public void setHorizontal(){
		getLEFTIntermediateVertex().setHorizontal(intermediateVertexMIDDLE.getX(),intermediateVertexMIDDLE.getY());
		getRIGHTIntermediateVertex().setHorizontal(intermediateVertexMIDDLE.getX(),intermediateVertexMIDDLE.getY());
		
	}
	
	public void setHorizontalReverse() {
		getLEFTIntermediateVertex().setHorizontal(intermediateVertexMIDDLE.getX(),intermediateVertexMIDDLE.getY());
		getRIGHTIntermediateVertex().setHorizontal(intermediateVertexMIDDLE.getX(),intermediateVertexMIDDLE.getY());
		Utils.debugMsg("BEFORE:"+"("+getLEFTIntermediateVertex().getX()+","+getLEFTIntermediateVertex().getY()+"), ("+getRIGHTIntermediateVertex().getX()+","+getRIGHTIntermediateVertex().getY()+").");
		double xR=getRIGHTIntermediateVertex().getX();
		double yR=getRIGHTIntermediateVertex().getY();
		double xL=getLEFTIntermediateVertex().getX();
		double yL=getLEFTIntermediateVertex().getY();
		getLEFTIntermediateVertex().setXYApply(xR,yR);
		getRIGHTIntermediateVertex().setXYApply(xL,yL);
		Utils.debugMsg("AFTER:"+"("+getLEFTIntermediateVertex().getX()+","+getLEFTIntermediateVertex().getY()+"), ("+getRIGHTIntermediateVertex().getX()+","+getRIGHTIntermediateVertex().getY()+").");
	}
	

	public BiochamGraph getGraph() {
		return graph;
	}

	public void setGraph(BiochamGraph graph) {
		this.graph = graph;
	}

	/*public void actionPerformed(ActionEvent e) {
		changeOrientationAction(e);
		
	}

	public void changeOrientationAction(ActionEvent e) {
		String cmd=e.getActionCommand();
		if(cmd.equals("verticalMiddleEdge")){		
			setVertical();		   
		}else if(cmd.equals("horizontalMiddleEdge")){
			setHorizontal();
		}else if(cmd.equals("edgeColor")){
			
		}		
	}*/

	public void setColor(Color green) {
		
		Map nested = new Hashtable();  
		for(int i=0;i<getComps().length;i++){			
			
			if(getComps()[i] instanceof IntermediateVertex){
				IntermediateVertex iv=(IntermediateVertex)getComps()[i];
				iv.setColor(green);
				iv=null;
			}else{				
		    	Map attributeMap1 = new Hashtable();
		    	BiochamGraphConstants.setLineColor(attributeMap1, green);
		    	BiochamGraphConstants.setBorderColor(attributeMap1, green);
		    	//BiochamGraphConstants.setBendable(attributeMap1,false);
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
	
	public String getName() {
		return "";
	}

	public void setLocation(Location location) {
		this.location = location;
	}
	public Location getLocation(){
		return location;
	}



	
	
	
	
}
