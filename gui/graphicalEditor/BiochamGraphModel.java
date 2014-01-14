package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.AttributeMap;
import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.DefaultGraphModel;
import org.jgraph.graph.DefaultPort;
import org.jgraph.graph.GraphModel;


import java.util.List;
import java.util.UUID;




public class BiochamGraphModel extends DefaultGraphModel{

	
	BiochamGraph graph;
	
	public BiochamGraphModel(BiochamGraph g) {
		super();
		graph=g;
	}

	public BiochamGraphModel(List roots, AttributeMap attributes) {
		super(roots, attributes);
	}
	
 
	public static boolean isValidEdge(Object edge) {
	
		if(edge instanceof StateTransition || edge instanceof Association || edge instanceof Dissociation || edge instanceof DefaultEdge || edge instanceof MiddleEdge){
			return true;
		}else{
			return false;
		}		
	}
	
	public static boolean isValidVertex(Object vertex) {
	
		if(vertex instanceof EMacromoleculeCell || vertex instanceof ENucleicAcidFeatureCell || vertex instanceof EComplexCell || vertex instanceof ESourceSink){
			return true;
		}else{
			return false;
		}		
	}
	
	public static boolean isGroup(GraphModel model, Object cell) {
		for (int i = 0; i < model.getChildCount(cell); i++) {
			if (!model.isPort(model.getChild(cell, i)))
				return true;
		}
		return false;
	}
	
	@Override
	protected Object cloneCell(Object cellObj) {
		if (cellObj instanceof DefaultGraphCell) {
			// Clones the cell
			DefaultGraphCell clone=null;
			DefaultGraphCell cell = (DefaultGraphCell) cellObj;			
			// Clones the user object
			BiochamEntityData data=null;
			if(cell.getUserObject() instanceof BiochamEntityData){
				clone = (DefaultGraphCell) cell.clone();
				BiochamEntityData d=(BiochamEntityData)cell.getUserObject();
				data =new BiochamEntityData(cell.getAttributes(),graph);
				data.setModulator(d.isModulator());
				data.setCompartment(d.getCompartment());
				data.setColor(d.getColor());
				data.setGraph(d.getGraph());
				data.setInitialConcentration(d.getInitialConcentration());
				data.setMoleculeState(d.getMoleculeState());
				data.setMultimerCardinality(d.getMultimerCardinality());
				data.setContainingMolecules(d.getContainingMolecules());		
				data.setModificationSites(d.getModificationSites());
				data.setNumberOfModifSites(d.getNumberOfModifSites());
				data.setCopy(true);
				((BiochamEntityData)cell.getUserObject()).setCopy(true);
				
				String name=d.getName();
				if(name.contains("(")){
					name=name.substring(0,name.indexOf("("));
				}
				DefaultGraphCell[] copies=GraphUtilities.getAllMoleculeCopies(graph,name);
				int size=copies.length;
				BiochamEntityData ddt;
				int max=d.getCopyInstance();
				for(int i=0;i<copies.length;i++){
					ddt=(BiochamEntityData)copies[i].getUserObject();
					if(ddt.getCopyInstance()>max){
						max=ddt.getCopyInstance();
					}					
				}
				ddt=null;
				copies=null;
								
				if(max<1){
					data.setCopyInstance(1);					
				}else{
					max+=1;
					data.setCopyInstance(max);				
				}
					
				name=d.getName();
				if(name.contains("(")){
					name=name.substring(0,name.indexOf("("));
				}
				data.setName(name+"("+data.getCopyInstance()+")");	
				data.setRepresentingName(d.getRepresentingName());
				data.setId(UUID.randomUUID());			
				clone.setUserObject(data);
			}else if(cell instanceof DefaultPort){
				clone = (DefaultGraphCell) cell.clone();				
			}else if(GraphUtilities.isBiochamReaction(cell)){
				clone = (DefaultGraphCell) cell.clone();
				clone.setUserObject(cloneUserObject(cell.getUserObject()));
			}else{
				clone = (DefaultGraphCell) cell.clone();
			}
			/*else{
				clone.setUserObject(cloneUserObject(cell.getUserObject()));
			}*/
			return clone;
		}
		return cellObj;
	}
	
}
