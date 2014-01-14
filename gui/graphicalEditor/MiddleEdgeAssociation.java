package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Hashtable;
import java.util.Map;

public class MiddleEdgeAssociation extends DefaultGraphCell implements CustomEdge{//, ActionListener{

	
	IntermediateVertex2 intermediateVertexLEFT;
	IntermediateVertex3 intermediateVertexRIGHT;
	IntermediateVertexAssoc intermediateVertexMIDDLE;
	BiochamGraph graph;
	Object[] comps;
	
	
	public MiddleEdgeAssociation(Object cell,BiochamGraph graph,double x, double y, boolean opposite) {
		
		this.graph=graph;
		DefaultGraphCell[] children=null;
		children=new DefaultGraphCell[3];	
					
		if(x!=0 && y!=0){
			intermediateVertexMIDDLE = new IntermediateVertexAssoc(graph,cell,x,y);
			
			if(opposite){
				intermediateVertexRIGHT  = new IntermediateVertex3(graph,x,y);
			}else{
				intermediateVertexLEFT   = new IntermediateVertex2(graph,x,y);	
			}
		}else{
			intermediateVertexMIDDLE = new IntermediateVertexAssoc(graph,cell);
			
			if(opposite){
				intermediateVertexRIGHT  = new IntermediateVertex3(intermediateVertexMIDDLE);
			}else{
				intermediateVertexLEFT   = new IntermediateVertex2(intermediateVertexMIDDLE);
			}
		}		
		
		if(opposite){
			children[0]=intermediateVertexRIGHT;
		}else{
			children[0]=intermediateVertexLEFT;
		}		
		children[1]=intermediateVertexMIDDLE;
	
		DefaultEdge edge1 = new DefaultEdge();
		if(opposite){		
			edge1.setSource(children[1].addPort());
			edge1.setTarget(children[0].addPort());
		}else{
			edge1.setSource(children[0].addPort());
			edge1.setTarget(children[1].addPort());	
		}
			
		children[2] = edge1;
				
		BiochamGraphConstants.setEditable(this.getAttributes(),true);
		BiochamGraphConstants.setDisconnectable(this.getAttributes(),false);
		BiochamGraphConstants.setGroupOpaque(this.getAttributes(),true);
		BiochamGraphConstants.setMoveable(this.getAttributes(),true);
		BiochamGraphConstants.setSelectable(this.getAttributes(),true);
		BiochamGraphConstants.setChildrenSelectable(this.getAttributes(),true);
				
		graph.getGraphLayoutCache().insertGroup(this,children);	
		this.setComps(children);	
	
	}
	
	public IntermediateVertex2 getLEFTIntermediateVertex() {
		return intermediateVertexLEFT;
	}
	public IntermediateVertexAssoc getMIDDLEIntermediateVertex() {
		return intermediateVertexMIDDLE;
	}
	
	public void setVerticalDown(){
		getLEFTIntermediateVertex().setVerticalDown(intermediateVertexMIDDLE.getX(),intermediateVertexMIDDLE.getY());	
	}
	
	public void setVerticalUp(){
		getLEFTIntermediateVertex().setVerticalUp(intermediateVertexMIDDLE.getX(),intermediateVertexMIDDLE.getY());	
	}
	
	public void setHorizontal(){
		getLEFTIntermediateVertex().setHorizontal(intermediateVertexMIDDLE.getX(),intermediateVertexMIDDLE.getY());
		
	}
	
	public void setHorizontalReverse() {		
		getLEFTIntermediateVertex().setBounds(intermediateVertexMIDDLE.getX()+30,intermediateVertexMIDDLE.getY()+3);		
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
			setVerticalUp();		   
		}else if(cmd.equals("horizontalMiddleEdge")){
			setHorizontal();
		}else if(cmd.equals("edgeColor")){
			
		}		
	}*/

	public void setColor(Color green) {
		
		Map nested = new Hashtable();  
		for(int i=0;i<getComps().length;i++){			
			
			if(getComps()[i] instanceof IntermediateVertexAssoc){
				IntermediateVertexAssoc iv=(IntermediateVertexAssoc)getComps()[i];
				iv.setColor(green);
			}else if(getComps()[i] instanceof IntermediateVertex2){
				IntermediateVertex2 iv=(IntermediateVertex2)getComps()[i];
				iv.setColor(green);
			}else if(getComps()[i] instanceof IntermediateVertex3){
				IntermediateVertex3 iv=(IntermediateVertex3)getComps()[i];
				iv.setColor(green);
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
	
	public String getName() {
		return "";
	}

	public IntermediateVertex3 getRIGHTIntermediateVertex() {
		return intermediateVertexRIGHT;
	}



	
	
	
}
