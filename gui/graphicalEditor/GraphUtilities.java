package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultEdge;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.GraphModel;
import org.jgraph.util.ParallelEdgeRouter;

import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.modelData.ParamTableInitConc;
import fr.inria.contraintes.biocham.modelData.ParamTableRules;
import fr.inria.contraintes.biocham.modelData.ParamTableRules.Rule;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.font.FontRenderContext;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeSet;
import java.util.UUID;
import java.util.Vector;




public class GraphUtilities {

	public static Color MACROMOLECULE_COLOR=new Color(113,198,113);
	public static Color NUCLEIC_ACID_FEATURE_COLOR=(new Color(61,89,171)).brighter();
	public static Color COMPLEX_COLOR=(new Color(240,128,128)).brighter();
	public static Color MODULATOR_COLOR=new Color(205,79,57);
	static FontRenderContext frc = new FontRenderContext(null, false, false);
	/*IS-EXISTS METHODS*/
	public static boolean isBiochamEntity(Object cell){
		if(cell instanceof EMacromoleculeCell || cell instanceof ENucleicAcidFeatureCell || cell instanceof EComplexCell){
			return true;
		}else{
			return false;
		}
	}	
	public static boolean isBiochamReaction(Object cell){
		if(cell instanceof StateTransition || cell instanceof Association || cell instanceof Dissociation || cell instanceof ReversibleStateTransition || cell instanceof ReversibleAssociation){
			return true;
		}else{
			return false;
		}
	}	
	public static boolean isBiochamReactionComponent(Object cell){
		if(cell instanceof StateTransition || cell instanceof Association || cell instanceof Dissociation || 
				cell instanceof BiochamEdge || cell instanceof DefaultEdge || cell instanceof IntermediateVertex ||
				cell instanceof IntermediateVertex2 || cell instanceof IntermediateVertex3 ||
				cell instanceof IntermediateVertexAssoc || cell instanceof IntermediateVertexDissoc || cell instanceof ReversibleStateTransition || cell instanceof ReversibleAssociation){
			return true;
		}else{
			return false;
		}
	}	
	public static boolean isBiochamCompartment(Object cell){
		if(cell instanceof ECompartmentCell){
			return true;
		}else{
			return false;
		}
	}	
	public static boolean isSourceSink(Object cell){
		if(cell instanceof ESourceSink){
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
	public static boolean existsInList(ArrayList names,String name){
		
		try{
			for(int i=0;i<names.size();i++){			
				if(name.equals(names.get(i).toString())){
					return true;
				}
			}
			return false;
		}catch(Exception e){
			return false;
		}
	}
	public static String findReversibleReaction(BiochamGraph graph,String r,String p){
		
		int size=graph.getReactionsModifications().size();
		
		for(int i=0;i<size;i++){
			
			String rule;
			
			if(graph.getReactionsModifications().get(i).getModifiedRule()==null){
				rule=graph.getReactionsModifications().get(i).getOriginalRule();								
			}else{
				rule=graph.getReactionsModifications().get(i).getModifiedRule();				
			}
			
			if(rule.substring(rule.indexOf("for")+3,rule.indexOf("=>")).trim().equals(p)){
				if(rule.substring(rule.indexOf(">")+1).trim().equals(r)){
					Utils.debugMsg("rule: "+rule+" has reversible rule: "+r+"=>"+p );
					return rule;
				}
			}
			
		}		
		return null;
	}
	public static ArrayList<String> hasReversibleReaction(BiochamGraph graph){		
		
		ArrayList<String> rets=new ArrayList<String>();
		int size=graph.getReactionsModifications().size();
		for(int i=0;i<size;i++){
			String rule;
			if(graph.getReactionsModifications().get(i).getModifiedRule()==null){
				rule=graph.getReactionsModifications().get(i).getOriginalRule();								
			}else{
				rule=graph.getReactionsModifications().get(i).getModifiedRule();				
			}
			if(!rule.contains("<=")){
				String hasReversible=findReversibleReaction(graph,rule.substring(rule.indexOf("for")+3,rule.indexOf("=>")).trim(),rule.substring(rule.indexOf(">")+1).trim());
				if(hasReversible!=null){
					//remove the both rules....
					// create a new reversible rule....
					rets.add(rule);
				}
			}			
		}	
		if(rets.size()>0){
			return rets;
		}else{
			return null;
		}
		
	}
	
	public static BiochamEntityData getDataFromCellName(BiochamGraph graph,String cell){
		return (BiochamEntityData)GraphUtilities.getCellByName(graph, cell).getUserObject();
	}
	
	public static boolean existsInGraph(BiochamGraph graph,String name){
		
		Object[] cells=getAllCells(graph);
		int len=cells.length;
		for(int i=0; i<len;i++){
			String n="";
			if(GraphUtilities.isBiochamReaction(((DefaultGraphCell)cells[i]))){
				n=((BiochamEdgeData)((DefaultGraphCell)cells[i]).getUserObject()).getName();
			}else{
				if(((BiochamEntityData)((DefaultGraphCell)cells[i]).getUserObject())!=null){
					n=((BiochamEntityData)((DefaultGraphCell)cells[i]).getUserObject()).getName();
				}				 
			}			
			if(n.equals(name) || n==name){
				n=null;
				cells=null;
				return true;
			}
			n=null;
		}		
		cells=null;
		return false;
	}
	/*public static boolean isResponsableVertex(DefaultGraphCell cell){
		if(isBiochamEntity(cell)){
			BiochamEntityData dt=getBiochamEntityDataFromCell(cell);
			if(dt!=null){
				int conn=dt.getNumberOfConnections();
				if(conn>1){
					return true;
				}else{
					return false;
				}
			}
			return false;
		}
		return false;
	}*/
	public static boolean isResponsableEntity(DefaultGraphCell cell){
		if(isBiochamEntity(cell)){
			BiochamEntityData dt=getBiochamEntityDataFromCell(cell);
			if(dt!=null){
				return dt.isResponsable();
			}
			return false;
		}
		return false;
	}
	
	/*GET METHODS*/
	public static BiochamEntityData getBiochamEntityDataFromCell(Object cell){
	
		if(isBiochamEntity(cell)){
			if(cell instanceof EMacromoleculeCell){				
				return ((BiochamEntityData)((EMacromoleculeCell)cell).getUserObject());
			}else if(cell instanceof ENucleicAcidFeatureCell){
				return ((BiochamEntityData)((ENucleicAcidFeatureCell)cell).getUserObject());
			}else if(cell instanceof EComplexCell){
				return ((BiochamEntityData)((EComplexCell)cell).getUserObject());
			}else {
				return null;
			}
		}else if(cell instanceof ESourceSink){
			return ((BiochamEntityData)((ESourceSink)cell).getUserObject());
		}else{
			return null;
		}
	}
	public static BiochamEdgeData getBiochamEdgeDataFromCell(Object cell){
		
		if(isBiochamReaction(cell)){
			if(cell instanceof StateTransition){				
				return ((BiochamEdgeData)((StateTransition)cell).getUserObject());
			}else if(cell instanceof Association){
				return ((BiochamEdgeData)((Association)cell).getUserObject());
			}else if(cell instanceof Dissociation){
				return ((BiochamEdgeData)((Dissociation)cell).getUserObject());
			}else if(cell instanceof ReversibleAssociation){
				return ((BiochamEdgeData)((ReversibleAssociation)cell).getUserObject());
			}else if(cell instanceof ReversibleStateTransition){
				return ((BiochamEdgeData)((ReversibleStateTransition)cell).getUserObject());
			}else {
				return null;
			}
		}else{
			return null;
		}
	}
	public static DefaultGraphCell getCellById(BiochamGraph g,UUID id){
		
		return getCellById(g, id.toString());
	}
		
	public static DefaultGraphCell getCellById(BiochamGraph g,String id){
				
		Object[] cells=getAllCells(g);
		int len=cells.length;
		for(int i=0; i<len;i++){
			String n="";
			if(GraphUtilities.isBiochamReaction(((DefaultGraphCell)cells[i]))){
				n=((BiochamEdgeData)((DefaultGraphCell)cells[i]).getUserObject()).getName();
			}else if(GraphUtilities.isBiochamEntity(cells[i]) || cells[i] instanceof ESourceSink){
				n=((BiochamEntityData)((DefaultGraphCell)cells[i]).getUserObject()).getId().toString();
			}
			if(n!=null && (n.equals(id.toString()) || n==id.toString())){
				DefaultGraphCell ret=((DefaultGraphCell)cells[i]);
				n=null;
				cells=null;
				return ret;
			}	
			n=null;
		}		
		cells=null;
		return null;
	}
	
	public static DefaultGraphCell getMoleculeById(BiochamGraph graph, UUID id) {
		Object[] cells=getAllMolecules(graph);
		int len=cells.length;
		for(int i=0; i<len;i++){
			String n=((BiochamEntityData)((DefaultGraphCell)cells[i]).getUserObject()).getId().toString();
			if(n.equals(id.toString()) || n==id.toString()){
				DefaultGraphCell ret=((DefaultGraphCell)cells[i]);
				n=null;
				cells=null;
				return ret;
			}	
			n=null;
		}		
		cells=null;
		return null;
	}
	
	public static DefaultGraphCell[] getAllMoleculeCopies(BiochamGraph graph, String name) {
		
		
		ArrayList<DefaultGraphCell> copies=new ArrayList<DefaultGraphCell>();
		Object[] cells=getAllMolecules(graph);		
		int len=cells.length;
		String n;
		for(int i=0;i<len;i++){						
			n=((BiochamEntityData)((DefaultGraphCell)cells[i]).getUserObject()).getName();			
			if(n.contains("(")){
				n=n.substring(0,n.indexOf("("));
			}
			if(n!=null){
				if(n.equals(name) || n==name){					
					copies.add(((DefaultGraphCell)cells[i]));
				}
			}	
		}	
		n=null;
		cells=null;
		DefaultGraphCell[] ret=new DefaultGraphCell[copies.size()];
		copies.toArray(ret);
		return ret;
	}
	
	public static Object[] getAllCells(BiochamGraph graph){
		//graph.setSelectionCells(graph.getRoots());
		return graph.getGraphLayoutCache().getCells(graph.getGraphLayoutCache().getRoots());//.getSelectionCells();
	}
		
	public static DefaultGraphCell getCellByName(BiochamGraph g,String name){
					
	
		Object[] cells=getAllCells(g);
		int len=cells.length;
		for(int i=0; i<len;i++){
			String n=null;
			if(GraphUtilities.isBiochamReaction(((DefaultGraphCell)cells[i]))){
				n=((BiochamEdgeData)((DefaultGraphCell)cells[i]).getUserObject()).getName();
			}else if(GraphUtilities.isBiochamEntity(cells[i])){
				if(((BiochamEntityData)((DefaultGraphCell)cells[i]).getUserObject())!=null){
						n=((BiochamEntityData)((DefaultGraphCell)cells[i]).getUserObject()).getName();	
				}				 
			}else if(GraphUtilities.isBiochamCompartment(cells[i])){
				n=((BiochamCompartmentData)((DefaultGraphCell)cells[i]).getUserObject()).getCompartmentName();
			}				
			if(n!=null && n.equals(name) || n==name){
				DefaultGraphCell ret=((DefaultGraphCell)cells[i]);
				
				n=null;
				cells=null;
				return ret;
			}
			n=null;
		}		
		cells=null;
		return null;
	}
	
	public static String getCellType(DefaultGraphCell cell){
		if(cell instanceof ENucleicAcidFeatureCell){
			return "NucleicAcidFeature";
		}else if(cell instanceof EComplexCell){
			return "Complex";
		}else if (cell instanceof EMacromoleculeCell) {
			return "Macromolecule";
		}else if(cell instanceof StateTransition){
			return "StateTransition";
		}else if(cell instanceof Association){
			return "Association";
		}else if(cell instanceof ESourceSink){
			return "Source/Sink";
		}else if(cell instanceof Dissociation){
			return "Dissociation";
		}else if(cell instanceof ECompartmentCell){
			return "Compartment";
		}else return null;
	}
		
	public static Object[] getAllMolecules(BiochamGraph graph){
	
		ArrayList<DefaultGraphCell> molecules=new ArrayList<DefaultGraphCell>();
		Object[] cells=getAllCells(graph);
		int len=cells.length;
		for(int i=0; i<len;i++){
			if(isBiochamEntity(cells[i])){
				molecules.add((DefaultGraphCell) cells[i]);
			}
		}
		cells=null;
		return molecules.toArray();
		
	}
	
	public static DefaultGraphCell[] getAllMoleculeCells(BiochamGraph graph){
		
		ArrayList<DefaultGraphCell> molecules=new ArrayList<DefaultGraphCell>();
		Object[] cells=getAllCells(graph);
		int len=cells.length;
		for(int i=0; i<len;i++){
			if(isBiochamEntity(cells[i])){
				molecules.add((DefaultGraphCell) cells[i]);
			}
		}
		cells=null;
		DefaultGraphCell[] cls=new DefaultGraphCell[molecules.size()];
		for(int i=0;i<cls.length;i++){
			cls[i]=molecules.get(i);
		}
		return cls;
		
	}
	
	public static DefaultGraphCell[] getAllReactions(BiochamGraph graph){
		ArrayList<DefaultGraphCell> molecules=new ArrayList<DefaultGraphCell>();
		Object[] cells=getAllCells(graph);
		int len=cells.length;
		for(int i=0; i<len;i++){
			if(isBiochamReaction(cells[i])){
				molecules.add((DefaultGraphCell) cells[i]);
			}
		}
		cells=null;
		DefaultGraphCell[] ret=new DefaultGraphCell[molecules.size()];
		for(int i=0;i<ret.length;i++){
			ret[i]=molecules.get(i);
		}
		return ret;		
	}
	public static ECompartmentCell[] getAllCompartments(BiochamGraph graph){
		ArrayList<ECompartmentCell> compartments=new ArrayList<ECompartmentCell>();
		Object[] cells=getAllCells(graph);
		int len=cells.length;
		for(int i=0; i<len;i++){
			if(isBiochamCompartment(cells[i])){
				compartments.add((ECompartmentCell) cells[i]);
			}
		}
		cells=null;
		ECompartmentCell[] ret=new ECompartmentCell[compartments.size()];
		for(int i=0;i<ret.length;i++){
			ret[i]=compartments.get(i);
		}
		return ret;		
	}	

	public static DefaultGraphCell[] getAllResponsableMolecules(BiochamGraph graph){
		ArrayList<DefaultGraphCell> molecules=new ArrayList<DefaultGraphCell>();
		
		return (DefaultGraphCell[]) molecules.toArray();
	}
	
	public static String[] getUniqueMoleculesNames(BiochamGraph graph) { 
		
		ArrayList<String> result = new ArrayList<String>(); 
		Object[] molecules=getAllMolecules(graph);
		for(int i=0;i<molecules.length;i++){
			BiochamEntityData dt=getBiochamEntityDataFromCell(molecules[i]);
			String name=dt.getName();
			/*if(dt.isCopy()){
				name+="("+dt.getCopyInstance()+")";
			}*/
			if(result.size()>0){
				if(!existsInList(result,name)){
					result.add(name);
				}	
			}else{
				result.add(name);
			}
			
			name=null;
		}
		molecules=null;
		String[] ret=new String[result.size()];
		for(int i=0;i<ret.length;i++){
			ret[i]=result.get(i);
		}
		result=null;
		return ret; 
	}
	public static ArrayList<String> getBelongReactions(DefaultGraphCell cell){
		if(isBiochamEntity(cell)){
			BiochamEntityData dt=getBiochamEntityDataFromCell(cell);
			if(dt!=null){
				return dt.getInvolvedInReactions();
			}
			return null;
		}
		return null;
	}
	public static DefaultGraphCell getReactionByName(BiochamGraph graph, String name){
		
		DefaultGraphCell c=getCellByName(graph, name);
		if(c!=null){
			return c; 
		}else{
			for(int i=0;i<graph.getReactionsModifications().size();i++){
				GraphReactions gr=graph.getReactionsModifications().get(i);
				if(gr.getOriginalRule().equals(name)){
					c=getCellByName(graph, gr.getModifiedRule());
					if(c!=null){
						return c;
					}
				}else if(gr.getModifiedRule()!=null && gr.getModifiedRule().equals(name)){
					c=getCellByName(graph, gr.getOriginalRule());
					if(c!=null){
						return c;
					} 
				}
				gr=null;
			}
			return null;
		}
	}
	
	public static ArrayList<DefaultGraphCell> getReactionWithReactants(BiochamGraph graph, ArrayList<String> names){
		
		ArrayList<DefaultGraphCell> results=new ArrayList<DefaultGraphCell>();
		DefaultGraphCell[] cells=GraphUtilities.getAllReactions(graph);
		for(int i=0;i<cells.length;i++){
			BiochamEdgeData dt=getBiochamEdgeDataFromCell(cells[i]);
			if(dt!=null){				
				if(dt.getReactants()!=null){
					ArrayList<String> reactants=new ArrayList<String>(dt.getReactants().size());
					for(int j=0;j<dt.getReactants().size();j++){
						reactants.add(dt.getReactants().get(j).getName());
					}
					boolean dontAdd=false;
					int contains=0;
					if(reactants.size()>0){
						for(int k=0;k<names.size();k++){
							if(reactants.contains(names.get(k))){
								contains++;
							}
							/*if(!reactants.contains(names.get(k))){
								dontAdd=true;
								break;
							}*/
						}
						if(contains!=names.size()){
							dontAdd=true;
						}
					}
					if(!dontAdd){
						results.add(cells[i]);
					}
					reactants.clear();
					reactants=null;
				}
			}		
			dt=null;
		}
		cells=null;
		return results;
	}
	public static ArrayList<DefaultGraphCell> getReactionWithProducts(BiochamGraph graph, ArrayList<String> names){
		
		ArrayList<DefaultGraphCell> results=new ArrayList<DefaultGraphCell>();
		DefaultGraphCell[] cells=GraphUtilities.getAllReactions(graph);
		for(int i=0;i<cells.length;i++){
			BiochamEdgeData dt=getBiochamEdgeDataFromCell(cells[i]);
			if(dt!=null){				
				if(dt.getProducts()!=null){
					ArrayList<String> products=new ArrayList<String>(dt.getProducts().size());
					for(int j=0;j<dt.getProducts().size();j++){
						products.add(dt.getProducts().get(j).getName());
					}
					boolean dontAdd=false;
					int contains=0;
					if(products.size()>0){
						for(int k=0;k<names.size();k++){
							if(products.contains(names.get(k))){
								contains++;
							}
							/*if(!products.contains(names.get(k))){
								dontAdd=true;
								break;
							}*/
						}
						if(contains!=names.size()){
							dontAdd=true;
						}
					}
					if(!dontAdd){
						results.add(cells[i]);
					}
					products.clear();
					products=null;
				}
			}		
			dt=null;
		}
		cells=null;
		return results;
	}
	
	public static ArrayList<DefaultGraphCell> getReactionWithReactantsAndProducts(BiochamGraph graph, ArrayList<String> r,ArrayList<String> p){
		
		ArrayList<DefaultGraphCell> results=new ArrayList<DefaultGraphCell>();
		ArrayList<DefaultGraphCell> withReactants=getReactionWithReactants(graph,r);
		ArrayList<DefaultGraphCell> withProducts=getReactionWithProducts(graph,p);
		if(withReactants!=null & withProducts!=null){
			for(int i=0;i<withReactants.size();i++){
				if(withProducts.contains(withReactants.get(i))){
					results.add(withReactants.get(i));
				}
			}
		}		
		return results;
	}

//	Check of it needs to de drawn in oposite direction (from right to left..)
	public static Position checkIfOpositeDirection(BiochamGraph graph,BiochamEdgeData userObject){
		
		ArrayList<String> r=new ArrayList<String>(); 
		for(int i=0;i<userObject.getReactants().size();i++){
			r.add(userObject.getReactants().get(i).getName());
		}
		ArrayList<String> p=new ArrayList<String>(); 
		for(int i=0;i<userObject.getProducts().size();i++){
			p.add(userObject.getProducts().get(i).getName());
		}
		ArrayList<DefaultGraphCell> res=GraphUtilities.getReactionWithReactantsAndProducts(graph, p, r);
		if(res!=null){
			if(res.size()>0){
				Position pos=null;
				if(res.get(0) instanceof StateTransition){
					StateTransition st=(StateTransition)res.get(0);
					pos=new Position(st.getMIDDLEIntermediateVertex().getX(),st.getMIDDLEIntermediateVertex().getY(),0,0);
				}else if(res.get(0) instanceof Association){
					Association st=(Association)res.get(0);
					pos=new Position(st.getMIDDLEIntermediateVertex().getX(),st.getMIDDLEIntermediateVertex().getY(),0,0);
				}else if(res.get(0) instanceof Dissociation){
					Dissociation st=(Dissociation)res.get(0);
					pos=new Position(st.getMIDDLEIntermediateVertex().getX(),st.getMIDDLEIntermediateVertex().getY(),0,0);
				}
				return pos;
			}
		}
		
		return null;
	}

	public static DefaultGraphCell getReactionByAproksName(BiochamGraph graph, String name){
		
		DefaultGraphCell cells[]=getAllReactions(graph);
		for(int i=0;i<cells.length;i++){
			String rn=((BiochamEdgeData)GraphUtilities.getBiochamEdgeDataFromCell(cells[i])).getName();
			if(rn.endsWith(name)){
				return cells[i];				
			}
		}
		return null;
	}
	
	public static MiddleVertex[] getAllReactionCenters(BiochamGraph graph){
		
		ArrayList<MiddleVertex> ret=new ArrayList<MiddleVertex>();		
		Object[] cells=getAllCells(graph);
		for(int i=0;i<cells.length;i++){
			if(cells[i] instanceof StateTransition){
				ret.add(((StateTransition)cells[i]).getMIDDLEIntermediateVertex());
			}else if(cells[i] instanceof Association){
				ret.add(((Association)cells[i]).getMIDDLEIntermediateVertex());
			}else if(cells[i] instanceof Dissociation){
				ret.add(((Dissociation)cells[i]).getMIDDLEIntermediateVertex());
			}else if(cells[i] instanceof ReversibleStateTransition){
				ret.add(((ReversibleStateTransition)cells[i]).getIntermediateVertexMIDDLE1());
			}else if(cells[i] instanceof ReversibleAssociation){
				ret.add(((ReversibleAssociation)cells[i]).getIntermediateVertexMIDDLE1());
			}
		}
		MiddleVertex[] all=new MiddleVertex[ret.size()];
		for(int i=0;i<ret.size();i++){
			all[i]=ret.get(i);
		}
		
		return all;
	}
	public static double getCellWidth(DefaultGraphCell cell){
		if(cell instanceof EMacromoleculeCell){
			return EMacromoleculeCell.WIDTH;
		}else if(cell instanceof ENucleicAcidFeatureCell){
			return ENucleicAcidFeatureCell.WIDTH;
		}else if(cell instanceof EComplexCell){
			return EComplexCell.WIDTH;
		}else if(cell instanceof ESourceSink){
			return 25;
		}else return EMacromoleculeCell.WIDTH;
	}
	public static double getCellHeight(DefaultGraphCell instance) {
		
		if(instance instanceof EMacromoleculeCell){
			return EMacromoleculeCell.HEIGHT;
		}else if(instance instanceof EComplexCell){
			return EComplexCell.HEIGHT;
		}else if(instance instanceof ENucleicAcidFeatureCell){
			return ENucleicAcidFeatureCell.HEIGHT;
		}else if(instance instanceof ESourceSink){
			return 25;
		}else return EMacromoleculeCell.HEIGHT;
		
	}	
	
	public static Location[] getAllReactionCentersPositions(BiochamGraph graph){
		MiddleVertex[] all=getAllReactionCenters(graph);
		Location[] locs=new Location[all.length];
		for(int i=0;i<all.length;i++){			
			locs[i]=new Location(all[i].getX(),all[i].getY());;
		}
		return locs;
	}
	
	public static Location getLastReactionCenterPosition(BiochamGraph graph){
		MiddleVertex[] all=getAllReactionCenters(graph);
		/*Location[] locs=new Location[all.length];
		for(int i=0;i<all.length;i++){			
			locs[i]=new Location(all[i].getX(),all[i].getY());;
		}*/
		if(all!=null){
			if(all.length>0){
				return new Location(all[all.length-1].getX(),all[all.length-1].getY());
			}
		}
		return null;
	}
	public static DefaultGraphCell[] getResponsables(BiochamGraph graph,String reactionName){
		
		DefaultGraphCell r=getReactionByName(graph, reactionName);
		BiochamEdgeData dt=getBiochamEdgeDataFromCell(r);
		ArrayList<DefaultGraphCell> respons=new ArrayList<DefaultGraphCell>();
		
		if(dt!=null){
		
			for(int i=0;i<dt.getMolecules().size();i++){
				DefaultGraphCell cell=getCellById(graph,dt.getMolecules().get(i));
				if(getBiochamEntityDataFromCell(cell)!=null){
					if(getBiochamEntityDataFromCell(cell).isResponsable()){
						respons.add(cell);
					}
				}
			}
			if(respons.size()>0){
				DefaultGraphCell[] ret=new DefaultGraphCell[respons.size()];
				for(int k=0;k<ret.length;k++){
					ret[k]=respons.get(k);
				}
				respons=null;
				return ret;
			}			
		}
		return null;
		
		
	}
	public static Location getDownFreeLocation(BiochamGraph graph){
		Location[] locs=getAllReactionCentersPositions(graph);
		double maxX=0, maxY=0;
		for(int i=0;i<locs.length;i++){
			if(maxX<locs[i].getX()){
				maxX=locs[i].getX();
			}
			if(maxY<locs[i].getY()){
				maxY=locs[i].getY();
			}
		}
		locs=null;		
		return new Location(maxX,maxY+150);
	}
	
	public static Location getFreeDiagonalLocation(BiochamGraph graph){
		Location[] locs=getAllReactionCentersPositions(graph);
		double maxX=0, maxY=0;
		for(int i=0;i<locs.length;i++){
			if(maxX<locs[i].getX()){
				maxX=locs[i].getX();
			}
			if(maxY<locs[i].getY()){
				maxY=locs[i].getY();
			}
		}
		locs=null;		
		return new Location(maxX+150,maxY+150);
	}
	
	public static Location getFreeYLocation(BiochamGraph graph){
	//	Location loc=getLastReactionCenterPosition(graph);
	//	return new Location(loc.getX(),loc.getY()+150);
		Location[] locs=getAllReactionCentersPositions(graph);
		double minX=0, maxY=0;
		if(locs!=null){
			if(locs.length>0){
				minX=locs[0].getX();
				for(int i=0;i<locs.length;i++){
					if(minX>locs[i].getX()){
						minX=locs[i].getX();
					}
					if(maxY<locs[i].getY()){
						maxY=locs[i].getY();
					}
				}
			}
			
		}
		locs=null;		
		return new Location(minX,maxY+150);
	}
	public static Location getFreeXLocation(BiochamGraph graph){
		Location loc=getLastReactionCenterPosition(graph);//getAllReactionCentersPositions(graph);
		//double maxX=0, maxY=0;
		/*for(int i=0;i<locs.length;i++){
			if(maxX<locs[i].getX()){
				maxX=locs[i].getX();
			}
			if(maxY<locs[i].getY()){
				maxY=locs[i].getY();
			}
		}
		locs=null;		*/
		if(loc!=null){
			return new Location(loc.getX()+350,loc.getY());
		}
		return loc;
	}
	
	public static double getFreeX(BiochamGraph graph){
		Location[] locs=getAllReactionCentersPositions(graph);
		double maxX=0;
		for(int i=0;i<locs.length;i++){
			if(maxX<locs[i].getX()){
				maxX=locs[i].getX();
			}			
		}
		locs=null;
		return maxX+80;
	}	
	public static double getFreeY(BiochamGraph graph){
		Location[] locs=getAllReactionCentersPositions(graph);
		double maxY=0;
		for(int i=0;i<locs.length;i++){
			if(maxY<locs[i].getY()){
				maxY=locs[i].getY();
			}		
		}
		locs=null;
		return maxY;
	}
	public static List getKeysFromValue(Map hm,Object value){
	    Set ref = hm.keySet();
	    Iterator it = ref.iterator();
	    List list = new ArrayList();

	    while (it.hasNext()) {
	      Object o = it.next(); 
	      if(hm.get(o).equals(value)) { 
	        list.add(o); 
	      } 
	    } 
	    return list;
	  }

	
	public static ArrayList<BiochamObject> sortMoleculesByInvolvedIn(BiochamGraph graph, ArrayList<BiochamObject> list){
		ArrayList<BiochamObject> newList=new ArrayList<BiochamObject>(list.size());
		DefaultGraphCell c=null;
		HashMap<BiochamObject,Integer> map=new HashMap<BiochamObject,Integer>();
		for(int i=0;i<list.size();i++){
			c=list.get(i).getInstance();
			if(c!=null){
				map.put(list.get(i),GraphUtilities.getBiochamEntityDataFromCell(c).getInvolvedInReactions().size());
			}
		}
		Collection set=map.values();
		int s1=set.size();
		Object[] obj=set.toArray();
		int s2=obj.length;
		/*for(int i=0;i<s2;i++){
			System.out.println("BEFORE: "+obj[i]);
		}*/
		Arrays.sort(obj);
		int s3=obj.length;
		/*for(int i=0;i<s3;i++){
			System.out.println("AFTER SORT: "+obj[i]);
		}*/
		TreeSet sorted=new TreeSet();
		int s4=sorted.size();
		for(int i=0;i<obj.length;i++){
			sorted.add(obj[i]);
		}
		for (Iterator it = sorted.iterator();it.hasNext();) {
			int val=(Integer)it.next();
			List<BiochamObject> l=getKeysFromValue(map,val);			
			s4=l.size();
			/*for(int i=0;i<s4;i++){
				System.out.println("value: "+val+","+l.get(i).parentName);
			}*/
			for(int j=0;j<l.size();j++){
				newList.add(l.get(j));
			}
		}
		
		/*for (Iterator iter = map.entrySet().iterator(); iter.hasNext();) {
			 Map.Entry entry =(Map.Entry)iter.next(); 
			 BiochamObject key =(BiochamObject)entry.getKey(); 
			 int value=(Integer)entry.getValue();
			 
		}*/
		return newList;
	}
	public static double calculateMaxHeight(ArrayList<BiochamObject> sources) {
		
		double max=0;		
		for(int i=0;i<sources.size();i++){
			BiochamEntityData dt=(BiochamEntityData)sources.get(i).getInstance().getUserObject();
			double height=0;
			if(dt.getSize()!=null){
				height=dt.getSize().getHeight();
			}else{
				height=GraphUtilities.getCellHeight(sources.get(i).getInstance());
			}
			if(max<height){
				max=height;
			}
			dt=null;
		}		
		return max;
	}


	public static double calculateMaxWidth(ArrayList<BiochamObject> sources) {
		
		double max=0;		
		for(int i=0;i<sources.size();i++){
			BiochamEntityData dt=(BiochamEntityData)sources.get(i).getInstance().getUserObject();
			double width=0;
			if(dt.getSize()!=null){
				width=dt.getSize().getWidth();
			}else{
				width=GraphUtilities.getCellWidth(sources.get(i).getInstance());
			}
			if(max<width){
				max=width;
			}
		}		
		return max;
	}

	
	public static void setAllMoleculesToSetByAlgo(BiochamGraph graph, boolean setByAlgo){
		Object[] mols=GraphUtilities.getAllMolecules(graph);
		for(int i=0;i<mols.length;i++){
			if(mols[i]!=null){
				if(mols[i] instanceof ESourceSink){
					try{
						((BiochamEntityData)((DefaultGraphCell)mols[i]).getUserObject()).setSetByAlgo(setByAlgo);
					}catch(Exception e){}
				}else{
					(GraphUtilities.getBiochamEntityDataFromCell(mols[i])).setSetByAlgo(setByAlgo);
				}
			}
		}
	}
	
	/*SET METHODS*/

		
	/*SET*/
	
	public static void setTargetsLocation(BiochamGraph graph,ArrayList<BiochamObject> targets, double xi, double yi) {
		
		double maxWidth=calculateMaxWidth(targets);
		double maxHeight=calculateMaxHeight(targets);
		double rectWidth=50;
		double rectHeight=targets.size()*maxHeight+50;
		double rX=xi+rectWidth+30;
		int siz=targets.size();
		double rY=yi-siz*(maxHeight+10)/2;
		
		for(int i=0;i<targets.size();i++){
			BiochamEntityData dt=(BiochamEntityData)targets.get(i).getInstance().getUserObject();
			
			if(!dt.isSetByAlgo()){
			
				double x=rX;
				if(i==0 || i==targets.size()-1){
					x-=40;
				}
				
				double y=rY+(maxHeight+40)*i;
				if(i==targets.size()-1){
					y-=10;
				}
				if(targets.size()==1){
					if(!(targets.get(i).getInstance() instanceof EComplexCell)){
						x=xi+50;
						if(GraphUtilities.isSourceSink(targets.get(i).getInstance())){
							maxHeight=25;
						}
						y=yi-maxHeight/2;
					}else{
						y+=20;
					}				
					
				}	
				if(x<0){
					x=10;
				}
				if(y<0){
					y=10;
				}
				dt.setPosition(new Position(x,y,0,0));
				targets.get(i).getInstance().setUserObject(dt);
				Map nested = new Hashtable();
				Map attributeMap1 = new Hashtable();			
				if(GraphUtilities.isSourceSink(targets.get(i).getInstance())){
					BiochamGraphConstants.setBounds(attributeMap1,new Rectangle2D.Double(x,y,((ESourceSink)targets.get(i).getInstance()).getWidth(),((ESourceSink)targets.get(i).getInstance()).getHeight()));
				}else{
					if(((DefaultGraphCell)targets.get(i).getInstance()) instanceof EMacromoleculeCell){
						((EMacromoleculeCell)((DefaultGraphCell)targets.get(i).getInstance())).setPosition(graph, x, y);
					}else if(((DefaultGraphCell)targets.get(i).getInstance()) instanceof ENucleicAcidFeatureCell){
						((ENucleicAcidFeatureCell)((DefaultGraphCell)targets.get(i).getInstance())).setPosition(graph, x, y);
					}else if(((DefaultGraphCell)targets.get(i).getInstance()) instanceof EComplexCell){
						((EComplexCell)((DefaultGraphCell)targets.get(i).getInstance())).setPosition(graph, x, y);
					}
					/*double w=BiochamGraphConstants.getBounds(((DefaultGraphCell)targets.get(i).getInstance()).getAttributes()).getWidth();
					double h=BiochamGraphConstants.getBounds(((DefaultGraphCell)targets.get(i).getInstance()).getAttributes()).getHeight();
					BiochamGraphConstants.setBounds(attributeMap1,new Rectangle2D.Double(x,y,w,h));*/
				}	
				nested.put(targets.get(i).getInstance(), attributeMap1 );
				if(graph!=null){
					graph.getGraphLayoutCache().edit(nested, null, null, null);
				}
			}
		}
	}
	public static void setSourcesLocation(BiochamGraph graph, ArrayList<BiochamObject> sources, double xi, double yi,boolean verification) {
		
		if(verification){
			double maxWidth=calculateMaxWidth(sources);
			double maxHeight=calculateMaxHeight(sources);
			double rectWidth=maxWidth+40;
			double rectHeight=sources.size()*maxHeight+40;		
			double rX=xi-rectWidth-30;
			double rY=yi-rectHeight/2;
			double x=50, y=50;
			for(int i=0;i<sources.size();i++){
				BiochamEntityData dt=(BiochamEntityData)sources.get(i).getInstance().getUserObject();
				//System.out.println("Positioning the mol: "+dt.getName());
				
				if(!dt.isSetByAlgo()){
					
					if(i==0){
						x=rX;
						y=rY+maxHeight*i+30*i;			
						if(sources.size()==1){
							x=xi-maxWidth-20;
							if(GraphUtilities.isSourceSink(sources.get(i).getInstance())){
								maxHeight=25;
							}
							y=yi-maxHeight/2;
						}				
						if(x<0){
							x=10;
						}
						if(y<0){
							y=10;
						}
						dt.setPosition(new Position(x,y,0,0));
						sources.get(i).getInstance().setUserObject(dt);
						Map nested = new Hashtable();
						Map attributeMap1 = new Hashtable();
						if(GraphUtilities.isSourceSink(sources.get(i).getInstance())){
							BiochamGraphConstants.setBounds(attributeMap1,new Rectangle2D.Double(x,y,((ESourceSink)sources.get(i).getInstance()).getWidth(),((ESourceSink)sources.get(i).getInstance()).getHeight()));
						}else{
							if(((DefaultGraphCell)sources.get(i).getInstance()) instanceof EMacromoleculeCell){
								((EMacromoleculeCell)((DefaultGraphCell)sources.get(i).getInstance())).setPosition(graph, x, y);
							}else if(((DefaultGraphCell)sources.get(i).getInstance()) instanceof ENucleicAcidFeatureCell){
								((ENucleicAcidFeatureCell)((DefaultGraphCell)sources.get(i).getInstance())).setPosition(graph, x, y);
							}else if(((DefaultGraphCell)sources.get(i).getInstance()) instanceof EComplexCell){
								((EComplexCell)((DefaultGraphCell)sources.get(i).getInstance())).setPosition(graph, x, y);
							}
							/*double w=BiochamGraphConstants.getBounds(((DefaultGraphCell)sources.get(i).getInstance()).getAttributes()).getWidth();
							double h=BiochamGraphConstants.getBounds(((DefaultGraphCell)sources.get(i).getInstance()).getAttributes()).getHeight();
							BiochamGraphConstants.setBounds(attributeMap1,new Rectangle2D.Double(x,y,w,h));*/
						}						
						nested.put(sources.get(i).getInstance(), attributeMap1 );
						if(graph!=null){
							graph.getGraphLayoutCache().edit(nested, null, null, null);
						}
					}else{						
						y+=maxHeight*i+30*i;		
						dt.setPosition(new Position(x,y,0,0));
						sources.get(i).getInstance().setUserObject(dt);
						Map nested = new Hashtable();
						Map attributeMap1 = new Hashtable();
						if(GraphUtilities.isSourceSink(sources.get(i).getInstance())){
							BiochamGraphConstants.setBounds(attributeMap1,new Rectangle2D.Double(x,y,((ESourceSink)sources.get(i).getInstance()).getWidth(),((ESourceSink)sources.get(i).getInstance()).getHeight()));
						}else{
							if(((DefaultGraphCell)sources.get(i).getInstance()) instanceof EMacromoleculeCell){
								((EMacromoleculeCell)((DefaultGraphCell)sources.get(i).getInstance())).setPosition(graph, x, y);
							}else if(((DefaultGraphCell)sources.get(i).getInstance()) instanceof ENucleicAcidFeatureCell){
								((ENucleicAcidFeatureCell)((DefaultGraphCell)sources.get(i).getInstance())).setPosition(graph, x, y);
							}else if(((DefaultGraphCell)sources.get(i).getInstance()) instanceof EComplexCell){
								((EComplexCell)((DefaultGraphCell)sources.get(i).getInstance())).setPosition(graph, x, y);
							}
							/*double w=BiochamGraphConstants.getBounds(((DefaultGraphCell)sources.get(i).getInstance()).getAttributes()).getWidth();
							double h=BiochamGraphConstants.getBounds(((DefaultGraphCell)sources.get(i).getInstance()).getAttributes()).getHeight();
							BiochamGraphConstants.setBounds(attributeMap1,new Rectangle2D.Double(x,y,w,h));*/
						}						
						nested.put(sources.get(i).getInstance(), attributeMap1 );
						if(graph!=null){
							graph.getGraphLayoutCache().edit(nested, null, null, null);
						}
					}
					
				}else{
					x=dt.getPosition().getX();
					y=dt.getPosition().getY();
				}
				
			}
		}else{
			setSourcesLocation(graph, sources, xi, yi);
		}
		
	}
	public static void setSourcesLocation(BiochamGraph graph, ArrayList<BiochamObject> sources, double xi, double yi) {
		
		double maxWidth=calculateMaxWidth(sources);
		double maxHeight=calculateMaxHeight(sources);
		double rectWidth=maxWidth+40;
		double rectHeight=sources.size()*maxHeight+40;		
		double rX=xi-rectWidth-30;
		double rY=yi-rectHeight/2;
				
		for(int i=0;i<sources.size();i++){
			BiochamEntityData dt=(BiochamEntityData)sources.get(i).getInstance().getUserObject();
			double x=rX;
			double y=rY+maxHeight*i+20*i;			
			if(sources.size()==1){
				x=xi-maxWidth-20;
				if(GraphUtilities.isSourceSink(sources.get(i).getInstance())){
					maxHeight=25;
				}
				y=yi-maxHeight/2;
			}				
			if(x<0){
				x=10;
			}
			if(y<0){
				y=10;
			}
			dt.setPosition(new Position(x,y,0,0));
			sources.get(i).getInstance().setUserObject(dt);
			Map nested = new Hashtable();
			Map attributeMap1 = new Hashtable();
			if(GraphUtilities.isSourceSink(sources.get(i).getInstance())){
				BiochamGraphConstants.setBounds(attributeMap1,new Rectangle2D.Double(x,y,((ESourceSink)sources.get(i).getInstance()).getWidth(),((ESourceSink)sources.get(i).getInstance()).getHeight()));
			}else{
				if(((DefaultGraphCell)sources.get(i).getInstance()) instanceof EMacromoleculeCell){
					((EMacromoleculeCell)((DefaultGraphCell)sources.get(i).getInstance())).setPosition(graph, x, y);
				}else if(((DefaultGraphCell)sources.get(i).getInstance()) instanceof ENucleicAcidFeatureCell){
					((ENucleicAcidFeatureCell)((DefaultGraphCell)sources.get(i).getInstance())).setPosition(graph, x, y);
				}else if(((DefaultGraphCell)sources.get(i).getInstance()) instanceof EComplexCell){
					((EComplexCell)((DefaultGraphCell)sources.get(i).getInstance())).setPosition(graph, x, y);
				}
				/*double w=BiochamGraphConstants.getBounds(((DefaultGraphCell)sources.get(i).getInstance()).getAttributes()).getWidth();
				double h=BiochamGraphConstants.getBounds(((DefaultGraphCell)sources.get(i).getInstance()).getAttributes()).getHeight();
				BiochamGraphConstants.setBounds(attributeMap1,new Rectangle2D.Double(x,y,w,h));*/
			}						
			nested.put(sources.get(i).getInstance(), attributeMap1 );
			if(graph!=null){
				graph.getGraphLayoutCache().edit(nested, null, null, null);
			}
		}
	}
	public static void setFirstModulatorLocation(BiochamGraph graph,DefaultGraphCell modulator,double xi, double yi) {
		
		double width=0,height=0;
		if(((BiochamEntityData)modulator.getUserObject()).getSize()!=null){
			if(((BiochamEntityData)modulator.getUserObject()).getSize().getWidth()>0){
				width=((BiochamEntityData)modulator.getUserObject()).getSize().getWidth();
			}
			if(((BiochamEntityData)modulator.getUserObject()).getSize().getHeight()>0){
				height=((BiochamEntityData)modulator.getUserObject()).getSize().getHeight();
			}
		}
		if(width==0){
			
			if(modulator instanceof EMacromoleculeCell){
				width=EMacromoleculeCell.WIDTH;
			}else if(modulator instanceof EComplexCell){
				width=EComplexCell.WIDTH;
			}else if(modulator instanceof ENucleicAcidFeatureCell){
				width=ENucleicAcidFeatureCell.WIDTH;
			}
		}	
		if(height==0){
			if(modulator instanceof EMacromoleculeCell){
				height=EMacromoleculeCell.HEIGHT;
			}else if(modulator instanceof EComplexCell){
				height=EComplexCell.HEIGHT;
			}else if(modulator instanceof ENucleicAcidFeatureCell){
				height=ENucleicAcidFeatureCell.HEIGHT;
			}
		}
		double x=xi+IntermediateVertex.WIDTH-width/2-4;//intermediateVertexMIDDLE.getX()
		double y=yi-(40+height);		
		((BiochamEntityData)modulator.getUserObject()).setPosition(new Position(x,y,0,0));
		
		Map nested = new Hashtable();
		Map attributeMap1 = new Hashtable();
		BiochamGraphConstants.setBounds(attributeMap1,new Rectangle2D.Double(x,y,width,height));			
		nested.put(modulator, attributeMap1 );
		if(graph!=null){
			graph.getGraphLayoutCache().edit(nested, null, null, null);
		}
		
		nested=null;
		attributeMap1=null;
	}
	
	
	
	public static void setUserObject(Object cell,Object obj){
		if(isBiochamEntity(cell)){
			if(cell instanceof EMacromoleculeCell){				
				((EMacromoleculeCell)cell).setUserObject(obj);
			}else if(cell instanceof ENucleicAcidFeatureCell){
				((ENucleicAcidFeatureCell)cell).setUserObject(obj);
			}else if(cell instanceof EComplexCell){
				((EComplexCell)cell).setUserObject(obj);
			}
		}
	}
	
	public static void setLineStyleOrthogonalSelectedCells(BiochamGraph g) {
		
	    Map nested = new Hashtable();     
	    Map attributeMap = new Hashtable();
	    for(Object o: g.getGraphLayoutCache().getCells(g.getGraphLayoutCache().getRoots())) {	    
	    	if(o instanceof DefaultEdge || o instanceof BiochamEdge){		    		
	    		BiochamGraphConstants.setBendable(attributeMap,false);
	    		BiochamGraphConstants.setRouting(attributeMap,ParallelEdgeRouter.getSharedInstance());
	    		BiochamGraphConstants.setLineStyle(attributeMap, BiochamGraphConstants.STYLE_ORTHOGONAL);
	    		nested.put(o, attributeMap);
	    		g.getGraphLayoutCache().edit(nested, null, null, null);
	    	}   
	    }
	  //  g.getGraphLayoutCache().edit(nested, null, null, null);
	    nested =null;
	    attributeMap = null;
	}
		
	public static void resetColors(BiochamGraph g) {
		Object[] cells=getAllMolecules(g);
		for(int i=0;i<cells.length;i++){
				BiochamEntityData d=GraphUtilities.getBiochamEntityDataFromCell(cells[i]);
				d.setColor(null);
				GraphUtilities.setUserObject(cells[i],d);
				d=null;
		}
		cells=null;
	}

	
	/* ADD REACTION TO MODEL (GRAPH) */
	/*ADD RULE*/
	
	public static void addReactionFromGraphToModel(BiochamGraph graph2, String value){
		
		
		if(value.endsWith(".")){
			value=value.substring(0,value.lastIndexOf("."));
		}
		BiochamModel model=graph2.getBiochamModel();
		((ParamTableRules)model.getRules().getParamTable()).setFromGraph(true);		
		ArrayList<Integer> al=new ArrayList<Integer>();		
		Vector<Rule> rules=((ParamTableRules)model.getRules().getParamTable()).getRules();
		for(int k=0;k<rules.size();k++){
			if(rules.get(k).getName().equals(value)){
				al.add(k);
			}
		}
		for(int i=0;i<al.size();i++){
			rules.removeElementAt(al.get(i));
		}
		al.clear();
		al=null;		
	  	String s="add_rules("+value+").\n";
	  	s+="list_molecules.\n";
	  	((ParamTableRules)model.getRules().getParamTable()).setFromGraph(true);	
	    model.sendToBiocham(s,"rules");	
	    ((ParamTableRules)model.getRules().getParamTable()).setFromGraph(true);
	
	    model=null;	   
	    rules.clear();
	    rules=null;
	    s=null;
	}
	
	
	public static void addReactionFromModelToGraph(BiochamGraph graph,String rule) {/*//BiochamGraph graph,Rule rule
		


		String collectReactants="", collectProducts="", modulator;			
		boolean isReversible=false;
		String reactionName=null, kinetics;
		ArrayList<DefaultGraphCell> reactants=new ArrayList<DefaultGraphCell>();
		ArrayList<DefaultGraphCell> products=new ArrayList<DefaultGraphCell>();			
		
		//catch the reaction label
		if(rule.contains(":")){
			String s=rule.substring(rule.indexOf(":")+1,rule.indexOf(":")+2);
			if(!s.equals(":")){
				reactionName=rule.substring(0,rule.indexOf(":"));
			}
			s=null;
		}
		//catch the reversibility property
		if(rule.contains("<")){
			isReversible=true;
		}
		if(isReversible){
			if(rule.contains("for")){
				collectReactants=rule.substring(rule.indexOf("for")+4,rule.indexOf("<="));
			}else{
				if(reactionName!=null){
					collectReactants=rule.substring(rule.indexOf(reactionName+":")+reactionName.length()+1,rule.indexOf("<="));
				}else{
					collectReactants=rule.substring(0,rule.indexOf("<="));
				}
			}
			
		}else{
			if(rule.contains("for")){
				collectReactants=rule.substring(rule.indexOf("for")+4,rule.indexOf("="));
			}else{
				if(reactionName!=null){
					collectReactants=rule.substring(rule.indexOf(reactionName+":")+reactionName.length()+1,rule.indexOf("="));
				}else{
					collectReactants=rule.substring(0,rule.indexOf("="));
				}
			}			
		}
		collectProducts=rule.substring(rule.lastIndexOf("=>")+2);		
		
		DefaultGraphCell modulatorCell=null;		
		
		//modulator or additional reactants&products
		if(rule.contains("=[")){
			modulator=rule.substring(rule.indexOf("=[")+2,rule.indexOf("]=")).trim();
			if(modulator.contains("=>")){				
				collectReactants+=";"+modulator.substring(0,modulator.indexOf("=")).trim();
				collectProducts+=";"+modulator.substring(modulator.indexOf(">")+1).trim();
				modulator=null;
			}else{
				
				if(graph!=null){
					modulatorCell=getCellByName(graph,modulator);
					
					if(modulatorCell==null){
						BiochamEntityData data1=new BiochamEntityData(graph);
						data1.setName(modulator);
						data1.setName(modulator);
						data1.setModulator(true);
						if(modulator.contains("*")){
							data1.setMultimerCardinality(Integer.valueOf(modulator.substring(0,modulator.indexOf("*"))));
						}
						if(modulator.contains("::")){
							String x=modulator.substring(modulator.lastIndexOf(":")+1);
							data1.setCompartment(x);
							if(modulator.substring(0,modulator.indexOf(":")).endsWith("}") || modulator.substring(0,modulator.indexOf(":")).contains("~$")){
								data1.setMoleculeState(MoleculeState.MODIFIED);
								data1.setModificationSites(modulator.substring(modulator.indexOf("~")+2,modulator.indexOf("}")).trim());
							}else{
								data1.setMoleculeState(MoleculeState.NONE);
							}
							x=null;
						}else if(modulator.contains("~")){
							data1.setMoleculeState(MoleculeState.MODIFIED);
							data1.setModificationSites(modulator.substring(modulator.indexOf("~")+2,modulator.indexOf("}")).trim());
						}else{
							data1.setMoleculeState(MoleculeState.NONE);
						}									
						if(modulator.contains("~")){
							data1.setMoleculeState(MoleculeState.MODIFIED);
						}
						if(modulator.contains("::")){
							data1.setCompartment(modulator.substring(modulator.lastIndexOf(":")+1));
						}
						if(modulator.startsWith("#")){
							modulatorCell=new ENucleicAcidFeatureCell(data1);
						}else{
							modulatorCell=new EMacromoleculeCell(data1);
						}
						graph.getGraphLayoutCache().insert(modulatorCell);
						data1=null;
					}else{
						BiochamEntityData data=(BiochamEntityData)modulatorCell.getUserObject();
						data.setModulator(true);
						modulatorCell.setUserObject(data);
						data=null;
					}
				}
			}
		}			
		
		//reaction kinetics
		if(rule.contains("for")){
			kinetics=rule.substring(0,rule.indexOf("for")-1).trim();
		}else{
			kinetics="1";
		}
		
		ArrayList<BiochamObject> sources=getReactantsfromRule(graph, collectReactants, reactants);		
		ArrayList<String> colReactants=new ArrayList<String>();		
		for(int i=0;i<sources.size();i++){
			if(sources.get(i).getStoichiometry()>1){
				colReactants.add((sources.get(i).getStoichiometry()+"*"+sources.get(i).getParentName()));	
			}else{
				colReactants.add(sources.get(i).getParentName());
				System.out.println(sources.get(i).getParentName());
			}
		}
		boolean targetsSet=false;
		ArrayList<BiochamObject> targets = null;
		
		//DISSOC-DEMULTIMERIZATION................
		if(sources.size()==1){
			if(((BiochamEntityData)sources.get(0).getInstance().getUserObject()).getMultimerCardinality()>1){
				DefaultGraphCell cell=null;
				if(graph!=null){
					String nm=((BiochamEntityData)sources.get(0).getInstance().getUserObject()).getRepresentingName();
					cell=getCellByName(graph,nm);
					if(cell==null){
						
						BiochamEntityData data1=new BiochamEntityData(graph);											
						int idx=((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).indexOf(nm.trim());
						if(idx>=0){
							data1.setInitialConcentration(((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).getValue(idx));
						}else{
							data1.setInitialConcentration("0.0");
						}
						if(nm.contains("::")){
							String x=nm.substring(nm.lastIndexOf(":")+1);
							data1.setCompartment(x);
							if(nm.substring(0,nm.indexOf(":")).endsWith("}") || nm.substring(0,nm.indexOf(":")).contains("~$")){
								data1.setMoleculeState(MoleculeState.MODIFIED);
								data1.setModificationSites(nm.substring(nm.indexOf("~")+2,nm.indexOf("}")).trim());
							}else{
								data1.setMoleculeState(MoleculeState.NONE);
							}
							x=null;
						}else if(nm.contains("~")){
							data1.setMoleculeState(MoleculeState.MODIFIED);
							data1.setModificationSites(nm.substring(nm.indexOf("~")+2,nm.indexOf("}")).trim());
						}else{
							data1.setMoleculeState(MoleculeState.NONE);
						}							
						data1.setName(nm.trim());
						if(nm.startsWith("#")){
							cell=new ENucleicAcidFeatureCell(data1);
						}else{
							cell=new EMacromoleculeCell(data1);
						}
						
						graph.getGraphLayoutCache().insert(cell);
					}
					
					targets=new ArrayList<BiochamObject>(1);
					targets.add(new BiochamObject(cell.addPort(),1,nm, cell));
					targetsSet=true;
				}else{
					targetsSet=false;
				}
			}else {
				targetsSet=false;
			}
		}else{
			targetsSet=false;
		}
		if(!targetsSet){
			targets=getProductsfromRule(graph, collectProducts,colReactants, products);
		}
		for(int i=0;i<targets.size();i++){
			System.out.println(targets.get(i).getParentName());
			
		}
		colReactants.clear();
		colReactants=null;
		
		//Association					
		ArrayList<UUID> molecules=new ArrayList<UUID>();		
		BiochamEdgeData edgeData=new BiochamEdgeData(graph,molecules);
		edgeData.setKinetics(kinetics);	
		if(reactionName!=null && reactionName!=""){
			edgeData.setName(reactionName);
		}
		edgeData.setReversible(isReversible);
		ArrayList<String> modulators=new ArrayList<String>();
		ArrayList<Object> toRemove=new ArrayList<Object>();
		int size=sources.size();
		if(size>1){
			for(int i=0;i<size;i++){				
				for(int j=0;j<targets.size();j++){
					String n1=sources.get(i).getParentName().trim();
					String n2=targets.get(j).getParentName().trim();					
					if(n1.equals(n2) && sources.get(i).getStoichiometry()<=1){
						if(sources.get(i).getStoichiometry()>1){
							modulators.add(sources.get(i).getParentName()+";"+sources.get(i).getStoichiometry());
						}else{
							modulators.add(sources.get(i).getParentName());
						}								
						targets.remove(j);
						toRemove.add(sources.get(i));
					}
					n1=null;
					n2=null;				
				}
			}
			for(int i=0;i<toRemove.size();i++){
				sources.remove(toRemove.get(i));
			}
		}
		
		
		
		if(targets.size()==1 && targets.get(0).getInstance() instanceof EComplexCell && sources.size()>1){
			
			edgeData.setModulator1(modulatorCell);
			edgeData.setReversibilityType(ReversibilityType.SINGLE);
			edgeData.setReversible(isReversible);
			for(int i=0;i<targets.size();i++){
				molecules.add(((BiochamEntityData)targets.get(i).getInstance().getUserObject()).getId());
			}
			for(int i=0;i<sources.size();i++){
				molecules.add(((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getId());
			}			
			if(modulatorCell!=null){
				molecules.add(((BiochamEntityData)modulatorCell.getUserObject()).getId());
			}
			edgeData.setMolecules(molecules);
			edgeData=GraphUtilities.setReactants(edgeData, sources);
			edgeData=GraphUtilities.setProducts(edgeData, targets);
			if(modulatorCell!=null){
				edgeData.getModulators1().add(new BiochamEnitity(((BiochamEntityData)modulatorCell.getUserObject()).getId(),((BiochamEntityData)modulatorCell.getUserObject()).getName()));	
			}
			
				
			edgeData.setStoichiometry(targets.get(0).getStoichiometry());
			for(int i=0;i<sources.size();i++){
				((BiochamEntityData)sources.get(i).getInstance().getUserObject()).setReactant(true);
			}
			((BiochamEntityData)targets.get(0).getInstance().getUserObject()).setProduct(true);
			if(modulatorCell!=null){
				((BiochamEntityData)modulatorCell.getUserObject()).setModulator(true);
			}
			
			DefaultGraphCell st;			
			if(isReversible){
				st=new ReversibleAssociation(edgeData,sources,targets.get(0),modulatorCell,graph,rule);
			}else{
				st=new Association(edgeData,sources,targets.get(0),modulatorCell,graph,rule);
			}
			
			
			
			if(modulators!=null){
				if(modulators.size()>0){
					for(int i=0;i<modulators.size();i++){
						if(modulators.get(i).contains(";")){							
							int stoich=1;
							try{
								stoich=Integer.valueOf(modulators.get(i).substring(modulators.indexOf(";")));
							}catch(Exception e){
							
							}
							
							if(st instanceof StateTransition){
								((StateTransition)st).addModulator(modulators.get(i).substring(0,modulators.get(i).indexOf(";")-1),stoich,false);
							}else{
								((ReversibleStateTransition)st).addModulator1(modulators.get(i).substring(0,modulators.get(i).indexOf(";")-1),stoich,false);
							}							
						}else{
							if(st instanceof StateTransition){
								((StateTransition)st).addModulator(modulators.get(i),1,false);
							}else{
								((ReversibleStateTransition)st).addModulator1(modulators.get(i),1,false);
							}
							
						}
					}
				}
			}
			
			BiochamGraphConstants.setEditable(st.getAttributes(),true);			
			BiochamGraphConstants.setDisconnectable(st.getAttributes(),false);
			BiochamGraphConstants.setMoveable(st.getAttributes(),true);
			BiochamGraphConstants.setSelectable(st.getAttributes(),true);
			graph.getGraphLayoutCache().insert(st);
			
			edgeData=null;st=null;
			
		//Dissociation		
		}else if(products.size()>1 && reactants.size()==1 && reactants.get(0) instanceof EComplexCell){
			
						
			edgeData.setModulator1(modulatorCell);
			edgeData.setReversibilityType(ReversibilityType.SINGLE);
			edgeData.setReversible(isReversible);
			
			for(int i=0;i<products.size();i++){
				molecules.add(((BiochamEntityData)products.get(i).getUserObject()).getId());
			}
			for(int i=0;i<reactants.size();i++){
				molecules.add(((BiochamEntityData)reactants.get(i).getUserObject()).getId());
			}			
			if(modulatorCell!=null){
				molecules.add(((BiochamEntityData)modulatorCell.getUserObject()).getId());
			}
			edgeData.setMolecules(molecules);	
			edgeData=GraphUtilities.setReactants(edgeData, sources);
			edgeData=GraphUtilities.setProducts(edgeData, targets);
			if(modulatorCell!=null){
				edgeData.getModulators1().add(new BiochamEnitity(((BiochamEntityData)modulatorCell.getUserObject()).getId(),((BiochamEntityData)modulatorCell.getUserObject()).getName()));	
			}
			
			ArrayList<DefaultGraphCell> srcs=new ArrayList<DefaultGraphCell>();
			for(int i=0;i<sources.size();i++){
				srcs.add(sources.get(i).getInstance());
			}
			edgeData.setReactants(srcs);			
			ArrayList<DefaultGraphCell> tgts=new ArrayList<DefaultGraphCell>();
			for(int i=0;i<targets.size();i++){
				tgts.add(targets.get(i).getInstance());
			}
			edgeData.setProducts(tgts);			
			ArrayList<DefaultGraphCell> mds=new ArrayList<DefaultGraphCell>();			
			if(modulatorCell!=null){
				mds.add(modulatorCell);
			}
			edgeData.setModulators(mds);			
			edgeData.setStoichiometry(sources.get(0).getStoichiometry());
			
			for(int i=0;i<targets.size();i++){
				((BiochamEntityData)targets.get(i).getInstance().getUserObject()).setProduct(true);
			}
			((BiochamEntityData)sources.get(0).getInstance().getUserObject()).setReactant(true);
			if(modulatorCell!=null){
				((BiochamEntityData)modulatorCell.getUserObject()).setModulator(true);
			}
			
			DefaultGraphCell st;			
			if(isReversible){
				st=new ReversibleDissociation(edgeData,sources.get(0),targets,modulatorCell ,graph,rule);
			}else{
				st=new Dissociation(edgeData,sources.get(0),targets,modulatorCell ,graph,rule);
			}
				
			
			BiochamGraphConstants.setEditable(st.getAttributes(),true);			
			BiochamGraphConstants.setDisconnectable(st.getAttributes(),false);
			BiochamGraphConstants.setMoveable(st.getAttributes(),true);
			BiochamGraphConstants.setSelectable(st.getAttributes(),true);
			graph.getGraphLayoutCache().insert(st);
			
			edgeData=null;st=null;	
			
		}else if(sources.size()==1 && targets.size()==1 && ((BiochamEntityData)targets.get(0).getInstance().getUserObject()).getMultimerCardinality()>0 && ((BiochamEntityData)sources.get(0).getInstance().getUserObject()).getMultimerCardinality()>0 && Math.abs(((BiochamEntityData)targets.get(0).getInstance().getUserObject()).getMultimerCardinality()-((BiochamEntityData)sources.get(0).getInstance().getUserObject()).getMultimerCardinality())>0){//targets.get(0).getInstance() instanceof EMacromoleculeCell){
			

			edgeData.setModulator1(modulatorCell);
			//edgeData.setReversibilityType(ReversibilityType.SINGLE);
			//edgeData.setReversible(isReversible);
			for(int i=0;i<targets.size();i++){
				molecules.add(((BiochamEntityData)targets.get(i).getInstance().getUserObject()).getId());
			}
			for(int i=0;i<sources.size();i++){
				molecules.add(((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getId());
			}			
			if(modulatorCell!=null){
				molecules.add(((BiochamEntityData)modulatorCell.getUserObject()).getId());
			}
			edgeData.setMolecules(molecules);	
			edgeData=GraphUtilities.setReactants(edgeData, sources);
			edgeData=GraphUtilities.setProducts(edgeData, targets);
			if(modulatorCell!=null){
				edgeData.getModulators1().add(new BiochamEnitity(((BiochamEntityData)modulatorCell.getUserObject()).getId(),((BiochamEntityData)modulatorCell.getUserObject()).getName()));	
			}
			
			
			ArrayList<DefaultGraphCell> srcs=new ArrayList<DefaultGraphCell>();
			for(int i=0;i<sources.size();i++){
				srcs.add(sources.get(i).getInstance());
			}
			edgeData.setReactants(srcs);			
			ArrayList<DefaultGraphCell> tgts=new ArrayList<DefaultGraphCell>();
			for(int i=0;i<targets.size();i++){
				tgts.add(targets.get(i).getInstance());
			}
			edgeData.setProducts(tgts);			
			ArrayList<DefaultGraphCell> mds=new ArrayList<DefaultGraphCell>();			
			if(modulatorCell!=null){
				mds.add(modulatorCell);
			}
			edgeData.setModulators(mds);
				
			edgeData.setStoichiometry(targets.get(0).getStoichiometry());
			for(int i=0;i<sources.size();i++){
				((BiochamEntityData)sources.get(i).getInstance().getUserObject()).setReactant(true);
			}
			((BiochamEntityData)targets.get(0).getInstance().getUserObject()).setProduct(true);
			if(modulatorCell!=null){
				((BiochamEntityData)modulatorCell.getUserObject()).setModulator(true);
			}
			
			DefaultGraphCell st = null;
			
			if(((BiochamEntityData)sources.get(0).getInstance().getUserObject()).getMultimerCardinality()>1){
				edgeData.setStoichiometry(((BiochamEntityData)sources.get(0).getInstance().getUserObject()).getMultimerCardinality());
				if(isReversible){								
					if(isReversible){
						st=new ReversibleDissociation(edgeData,sources.get(0),targets,modulatorCell,graph,rule);
					}else{
						st=new Dissociation(edgeData,sources.get(0),targets,modulatorCell,graph,rule);
					}
				}				
			}else{
				if(isReversible){								
					if(isReversible){
						st=new ReversibleAssociation(edgeData,sources,targets.get(0),modulatorCell,graph,rule);
					}else{
						st=new Association(edgeData,sources,targets.get(0),modulatorCell,graph,rule);
					}
				}
				
			}
			
			
			if(modulators!=null){
				if(modulators.size()>0){
					for(int i=0;i<modulators.size();i++){
						if(modulators.get(i).contains(";")){							
							int stoich=1;
							try{
								stoich=Integer.valueOf(modulators.get(i).substring(modulators.indexOf(";")));
							}catch(Exception e){
							
							}	
							if(st instanceof Association){
								((Association)st).addModulator(modulators.get(i).substring(0,modulators.get(i).indexOf(";")-1),stoich,false);
							}else if(st instanceof Dissociation){
								((Dissociation)st).addModulator(modulators.get(i).substring(0,modulators.get(i).indexOf(";")-1),stoich,false);
							}else if(st instanceof ReversibleAssociation){
								((ReversibleAssociation)st).addModulator1(modulators.get(i).substring(0,modulators.get(i).indexOf(";")-1),stoich,false);
							}							
						}else{
							if(st instanceof Association){
								((Association)st).addModulator(modulators.get(i),1,false);
							}else if(st instanceof Dissociation){
								((Dissociation)st).addModulator(modulators.get(i),1,false);
							}else if(st instanceof ReversibleAssociation){
								((ReversibleAssociation)st).addModulator1(modulators.get(i),1,false);
							}						
						}
					}
				}
			}
			
			BiochamGraphConstants.setEditable(st.getAttributes(),true);			
			BiochamGraphConstants.setDisconnectable(st.getAttributes(),false);
			BiochamGraphConstants.setMoveable(st.getAttributes(),true);
			BiochamGraphConstants.setSelectable(st.getAttributes(),true);
			graph.getGraphLayoutCache().insert(st);
			
			edgeData=null;st=null;
				
			
		}else{			
		
			edgeData.setModulator1(modulatorCell);
			edgeData.setReversibilityType(ReversibilityType.SINGLE);
			edgeData.setReversible(isReversible);
			for(int i=0;i<targets.size();i++){
				molecules.add(((BiochamEntityData)targets.get(i).getInstance().getUserObject()).getId());
			}
			for(int i=0;i<sources.size();i++){
				molecules.add(((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getId());
			}			
			if(modulatorCell!=null){
				molecules.add(((BiochamEntityData)modulatorCell.getUserObject()).getId());
			}
			edgeData.setMolecules(molecules);
			
			edgeData=GraphUtilities.setReactants(edgeData, sources);
			edgeData=GraphUtilities.setProducts(edgeData, targets);
			if(modulatorCell!=null){
				edgeData.getModulators1().add(new BiochamEnitity(((BiochamEntityData)modulatorCell.getUserObject()).getId(),((BiochamEntityData)modulatorCell.getUserObject()).getName()));	
			}
			
			if(modulators!=null){
				for(int i=0;i<modulators.size();i++){
					DefaultGraphCell cl=getCellByName(graph,modulators.get(i));
					if(cl!=null){
						edgeData.getModulators1().add(new BiochamEnitity(((BiochamEntityData)cl.getUserObject()).getId(),((BiochamEntityData)cl.getUserObject()).getName()));
					}
				}	
			}
			
			for(int k=0;k<sources.size();k++){
				((BiochamEntityData)sources.get(k).getInstance().getUserObject()).setReactant(true);
			}
			for(int k=0;k<targets.size();k++){
				((BiochamEntityData)targets.get(k).getInstance().getUserObject()).setProduct(true);
			}		
			
			DefaultGraphCell st;			
			if(isReversible){
				st=new ReversibleStateTransition(edgeData,sources,targets,modulatorCell,null,graph,false,rule.trim());
			}else{
				st=new StateTransition(edgeData,sources,targets,modulatorCell,null,graph,false,rule.trim());
			}
			
		
			
			
			if(modulators!=null){
				if(modulators.size()>0){
					for(int i=0;i<modulators.size();i++){
						if(modulators.get(i).contains(";")){							
							int stoich=1;
							try{
								stoich=Integer.valueOf(modulators.get(i).substring(modulators.indexOf(";")));
							}catch(Exception e){}
							if(st instanceof StateTransition){
								((StateTransition)st).addModulator(modulators.get(i).substring(0,modulators.get(i).indexOf(";")-1),stoich,false);
							}else{
								((ReversibleStateTransition)st).addModulator1(modulators.get(i).substring(0,modulators.get(i).indexOf(";")-1),stoich,false);
							}							
						}else{
							if(st instanceof StateTransition){
								((StateTransition)st).addModulator(modulators.get(i),1,false);
							}else{
								((ReversibleStateTransition)st).addModulator1(modulators.get(i),1,false);
							}							
						}
					}
				}
			}
			BiochamGraphConstants.setEditable(st.getAttributes(),true);			
			BiochamGraphConstants.setDisconnectable(st.getAttributes(),false);
			BiochamGraphConstants.setMoveable(st.getAttributes(),true);
			BiochamGraphConstants.setSelectable(st.getAttributes(),true);
			
			
			graph.getGraphLayoutCache().insert(st);
			
			edgeData=null;st=null;			
			
		}		
		collectReactants=null; collectProducts=null; modulator=null;	
		reactionName=null;kinetics=null;
		reactants.clear(); reactants=null;
		products.clear();products=null;
		modulatorCell=null;
		sources.clear();sources=null;
		targets.clear();targets=null;
		molecules.clear();molecules=null;
		modulators.clear();modulators=null;
		toRemove.clear();toRemove=null;
		edgeData=null;
		
	*/}

	
	/*Utilities methods for adding reaction to(from) model(graph).........*/
	private static ArrayList<BiochamObject> getProductsfromRule(BiochamGraph graph, String collectProducts,ArrayList<String> collectedReactants, ArrayList<DefaultGraphCell> products) {
		
		
		ArrayList<BiochamObject> sources=new ArrayList<BiochamObject>();		
		if(collectProducts.equals("_")){			
			BiochamEntityData data=new BiochamEntityData(graph);
			data.setName("Source/Sink");
			DefaultGraphCell cell=new ESourceSink(data);
			cell.setUserObject(data);
			products.add(cell);
			sources.add(new BiochamObject(cell.addPort(),data.getMultimerCardinality(),"Source/Sink",cell));
			graph.getGraphLayoutCache().insert(cell);
			//data=null;
			//cell=null;
		}else{			
		
			StringTokenizer st0=new StringTokenizer(collectProducts,";");
			while(st0.hasMoreTokens()){
				
				
				//COUNT IT.................
				String name=st0.nextToken().trim();
				boolean isModulator=false;
				for(int k=0;k<collectedReactants.size();k++){
					if(collectedReactants.get(k).trim().equals(name.trim())){
							isModulator=true;
							break;
					}
				}
				if(!isModulator){
					
					DefaultGraphCell cell=null;
					if(graph!=null){
						cell=getCellByName(graph,name);
						if(cell!=null){					
							products.add(cell);
							BiochamEntityData data=(BiochamEntityData)cell.getUserObject();
							sources.add(new BiochamObject(cell.addPort(),data.getMultimerCardinality(),data.getName(),cell));
							data=null;
						}else{
		
							if(name.equals("_")){
								BiochamEntityData data=new BiochamEntityData(graph);
								data.setName("Source/Sink");
								cell=new ESourceSink(data);
								products.add(cell);
								sources.add(new BiochamObject(cell.addPort(),0,data.getName(),cell));
								graph.getGraphLayoutCache().insert(cell);
								data=null;
								
							}else if(name.contains("+")){
								
								StringTokenizer st1=new StringTokenizer(name,"+");
								while(st1.hasMoreTokens()){
								
									String n=st1.nextToken().trim();							
									//Is it a COMPLEX?????????? If it's a complex, it already exists in the graph...
									if(n.contains("-")){
										cell=getCellByName(graph,n);
										if(cell!=null){
											products.add(cell);
											BiochamEntityData data=(BiochamEntityData)cell.getUserObject();
											sources.add(new BiochamObject(cell.addPort(),data.getMultimerCardinality(),data.getName(),cell));
											data=null;
										}else{
											if(collectedReactants!=null){
												
												ArrayList<ContainingMolecule> included=new ArrayList<ContainingMolecule>();
												
												for(int i=0;i<collectedReactants.size();i++){
													if(n.contains(collectedReactants.get(i))){
														cell=getCellByName(graph,collectedReactants.get(i));
														
														String type=getCellType(cell);														
														String nm=((BiochamEntityData)cell.getUserObject()).getName();
														if(((BiochamEntityData)cell.getUserObject()).getMultimerCardinality()>1){
															if(((BiochamEntityData)cell.getUserObject()).getName().contains("-")){
																String tmpStr=((BiochamEntityData)cell.getUserObject()).getName().substring(((BiochamEntityData)cell.getUserObject()).getName().lastIndexOf("-")+1,((BiochamEntityData)cell.getUserObject()).getName().length());	
																nm=((BiochamEntityData)cell.getUserObject()).getName().substring(0,((BiochamEntityData)cell.getUserObject()).getName().indexOf(tmpStr))+tmpStr;
																tmpStr=null;
															}
														}
														if(cell!=null){
															BiochamEntityData dt=((BiochamEntityData)cell.getUserObject());
															if(dt.getRepresentingName()==null){
																boolean set=false;
																if(dt.getMoleculeState()==MoleculeState.MODIFIED){
																	if(dt.getName().contains("~")){
																		dt.setRepresentingName(dt.getName().substring(0,dt.getName().indexOf("~")));
																		set=true;
																	}					
																}
																if(!set){
																	dt.setRepresentingName(dt.getName());
																}
															}
															((BiochamEntityData)cell.getUserObject()).setRepresentingName(nm);
															((BiochamEntityData)cell.getUserObject()).setMoleculeType(type);
															included.add(new ContainingMolecule(((BiochamEntityData)cell.getUserObject()).getName(),((BiochamEntityData)cell.getUserObject()).getMoleculeState(),((BiochamEntityData)cell.getUserObject()).getMultimerCardinality(),((BiochamEntityData)cell.getUserObject()).isModulator(),type,((BiochamEntityData)cell.getUserObject()).getInitialConcentration(),nm,((BiochamEntityData)cell.getUserObject()).getId(),cell));
														}
														type=null;
														nm=null;
													}
												}
												
												BiochamEntityData data1=new BiochamEntityData(graph);
												int idx=((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).indexOf(n.trim());
												if(idx>=0){
													data1.setInitialConcentration(((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).getValue(idx));
												}else{
													data1.setInitialConcentration("0.0");
												}
												data1.setName(n.trim());
												if(n.contains("::")){
													String x=n.substring(n.lastIndexOf(":")+1);
													data1.setCompartment(x);
													if(n.substring(0,n.indexOf(":")).endsWith("}") || n.substring(0,n.indexOf(":")).contains("~$")){
														data1.setMoleculeState(MoleculeState.MODIFIED);
														data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
													}else{
														data1.setMoleculeState(MoleculeState.NONE);
													}
													x=null;
												}else if(n.contains("~")){
													data1.setMoleculeState(MoleculeState.MODIFIED);
													data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
												}else{
													data1.setMoleculeState(MoleculeState.NONE);
												}											
												
												data1.setContainingMolecules(included);												
												cell=new EComplexCell(data1);
												cell.setUserObject(data1);
												products.add(cell);
												sources.add(new BiochamObject(cell.addPort(),1,data1.getName(),cell));//data1.getMultimerCardinality()
												graph.getGraphLayoutCache().insert(cell);
												
												included=null;
												data1=null;
											}
										}
									}
									// Its not a Complex, 
									else{								
										ostalo(sources,graph, products, n,collectedReactants);								
									}
									n=null;
								}
								st1=null;
							}else{
								ostalo(sources,graph, products, name,collectedReactants);
							}						
						}
					}
					cell=null;
				}
				name=null;
			}//finishes while(has tokens)....
			st0=null;
		}
		return sources;
	}
	private static ArrayList<BiochamObject> getReactantsfromRule(BiochamGraph graph, String collectReactants, ArrayList<DefaultGraphCell> reactants) {
		
		ArrayList<BiochamObject> sources=new ArrayList<BiochamObject>();
		
		if(collectReactants.equals("_")){
			
			BiochamEntityData data=new BiochamEntityData(graph);
			data.setName("Source/Sink");
			DefaultGraphCell cell=new ESourceSink(data);
			cell.setUserObject(data);
			reactants.add(cell);
			sources.add(new BiochamObject(cell.addPort(),data.getMultimerCardinality(),"Source/Sink",cell));
			graph.getGraphLayoutCache().insert(cell);
			data=null;
			cell=null;
		}else{
		
			StringTokenizer st0=new StringTokenizer(collectReactants,";");
			while(st0.hasMoreTokens()){
				
				String name=st0.nextToken().trim();				
				DefaultGraphCell cell=null;
				if(graph!=null){
					cell=getCellByName(graph,name);
					if(cell!=null){					
						reactants.add(cell);
						BiochamEntityData data=(BiochamEntityData)cell.getUserObject();
						sources.add(new BiochamObject(cell.addPort(),data.getMultimerCardinality(),data.getName(),cell));
						data=null;
					}else{
	
						if(name.equals("_")){
							BiochamEntityData data=new BiochamEntityData(graph);
							data.setName("Source/Sink");
							cell=new ESourceSink(data);
							reactants.add(cell);
							sources.add(new BiochamObject(cell.addPort(),0,data.getName(),cell));
							graph.getGraphLayoutCache().insert(cell);
							data=null;						
							
						}else if(name.contains("+")){
							
							StringTokenizer st1=new StringTokenizer(name,"+");
							String tokenBefore="";
							int counter=1;
							while(st1.hasMoreTokens()){
														
								String n=st1.nextToken().trim();
								if(n.equals(tokenBefore)){
									counter++;
									//int k=st1.countTokens();
									if(!st1.hasMoreTokens()){
										if(counter>1){
											
											cell=getCellByName(graph,tokenBefore);
											//((BiochamEntityData)cell.getUserObject()).setMultimerCardinality(counter);
											sources.get(sources.size()-1).setStoichiometry(counter);
											counter=0;
											
										}
									}
								}else{	
									//tokenBefore=n;
									if(counter>1){
										
										cell=getCellByName(graph,tokenBefore);
										//((BiochamEntityData)cell.getUserObject()).setMultimerCardinality(counter);
										sources.get(sources.size()-1).setStoichiometry(counter);
										counter=0;
										
									}
									tokenBefore=n;
									//Is it a COMPLEX?????????? If it's a complex, it already exists in the graph...
									if(n.contains("-")){
										
										cell=getCellByName(graph,n);
										if(cell!=null){
											reactants.add(cell);
											BiochamEntityData data=(BiochamEntityData)cell.getUserObject();
											if(n.contains("*")){																
												String card=n.substring(0,n.indexOf("*"));
												int cardinal=0;
												try{
													cardinal=Integer.valueOf(card);
												}catch(Exception e){
													
												}finally{
													sources.add(new BiochamObject(cell.addPort(),cardinal,data.getName(),cell));
												}
											}else{
												sources.add(new BiochamObject(cell.addPort(),1,data.getName(),cell));
											}
											data=null;
										}else if(n.contains("(") && n.contains("*")){
											
											String nm1=n.substring(n.indexOf("(")+1,n.indexOf(")"));
											int cardin=0;
											try{
												cardin=Integer.valueOf(n.substring(0,n.indexOf("*")));											
											}catch(Exception e){}
											cell=getCellByName(graph,nm1);
											if(cell!=null){
												reactants.add(cell);
												BiochamEntityData data=(BiochamEntityData)cell.getUserObject();
												sources.add(new BiochamObject(cell.addPort(),cardin,data.getName(),cell));
												data=null;
											}
											nm1=null;
										}
										
										else{
											//System.out.println("MISSING COMPLEX: "+n);
											BiochamEntityData data1=new BiochamEntityData(graph);											
											int idx=((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).indexOf(n.trim());
											if(idx>=0){
												data1.setInitialConcentration(((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).getValue(idx));
											}else{
												data1.setInitialConcentration("0.0");
											}
											if(n.contains("::")){
												String x=n.substring(n.lastIndexOf(":")+1);
												data1.setCompartment(x);
												if(n.substring(0,n.indexOf(":")).endsWith("}") || n.substring(0,n.indexOf(":")).contains("~$")){
													data1.setMoleculeState(MoleculeState.MODIFIED);
													data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
												}else{
													data1.setMoleculeState(MoleculeState.NONE);
												}
												x=null;
											}else if(n.contains("~")){
												data1.setMoleculeState(MoleculeState.MODIFIED);
												data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
											}else{
												data1.setMoleculeState(MoleculeState.NONE);
											}						
											data1.setName(n.trim());
											if(n.startsWith("#")){
												cell=new ENucleicAcidFeatureCell(data1);
											}else{
												cell=new EMacromoleculeCell(data1);
											}
											
											reactants.add(cell);	
											graph.getGraphLayoutCache().insert(cell);
											sources.add(new BiochamObject(cell.addPort(),data1.getMultimerCardinality(),data1.getName(),cell));
											data1=null;											
										}								
									}
									// Its not a Complex, 
									else{								
										ostalo(sources,graph, reactants, n,null);								
									}
								}					
							
								n=null;
							}	
							tokenBefore=null;
							st1=null;
						}
						else{
							ostalo(sources,graph, reactants, name,null);
						}
					}
				}
				name=null;
				cell=null;
			}	
			st0=null;
		}	
		return sources;
	}
	private static void ostalo(ArrayList<BiochamObject> sources, BiochamGraph graph, ArrayList<DefaultGraphCell> reactants, String n, ArrayList<String> collectedReactants) {
		
		
		DefaultGraphCell cell;
		
		
		//is it a simple or multiplied GENE??????
		if(n.startsWith("#") && !n.contains("-")){
			
			if(graph!=null){
				
				cell=getCellByName(graph,n.trim());
				if(cell!=null){
					reactants.add(cell);
					BiochamEntityData data=(BiochamEntityData)cell.getUserObject();
					sources.add(new BiochamObject(cell.addPort(),data.getMultimerCardinality(),data.getName(),cell));
					data=null;
				}else{

					BiochamEntityData data1=new BiochamEntityData(graph);
					if(n.contains("::")){
						String x=n.substring(n.lastIndexOf(":")+1);
						data1.setCompartment(x);
						if(n.substring(0,n.indexOf(":")).endsWith("}") || n.substring(0,n.indexOf(":")).contains("~$")){
							data1.setMoleculeState(MoleculeState.MODIFIED);
							data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
						}else{
							data1.setMoleculeState(MoleculeState.NONE);
						}
						x=null;
					}else if(n.contains("~")){
						data1.setMoleculeState(MoleculeState.MODIFIED);
						data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
					}else{
						data1.setMoleculeState(MoleculeState.NONE);
					}						
					/*if(n.contains("~")){
						//phoshoSites=n.substring(n.indexOf("~")+2,n.indexOf("}"));
						((BiochamEntityData)data1).setMoleculeState(MoleculeState.MODIFIED);
					}else{
						((BiochamEntityData)data1).setMoleculeState(MoleculeState.NONE);
					}									*/
					((BiochamEntityData)data1).setName(n.trim());
					/*if(n.contains("::")){
						if(n.substring(n.lastIndexOf(":")).contains("~")){
							((BiochamEntityData)data1).setCompartment(n.substring(n.lastIndexOf(":"),n.indexOf("~")));
						}else{
							((BiochamEntityData)data1).setCompartment(n.substring(n.lastIndexOf(":")));
						}
					}		*/	
					int idx=((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).indexOf(n.trim());
					if(idx>=0){
						data1.setInitialConcentration(((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).getValue(idx));
					}else{
						data1.setInitialConcentration("0.0");
					}				
					if(n.startsWith("#")){
						((BiochamEntityData)data1).setMultimerCardinality(0);
					}else{
						((BiochamEntityData)data1).setMultimerCardinality(Integer.valueOf(n.substring(0,n.indexOf("#"))));
					}									
					cell=new ENucleicAcidFeatureCell(data1);
					reactants.add(cell);
					graph.getGraphLayoutCache().insert(cell);
					sources.add(new BiochamObject(cell.addPort(),data1.getMultimerCardinality(),data1.getName(),cell));
					
					data1=null;
					
				}
			}
		}else if(n.contains("+")){
			
			StringTokenizer stPlus=new StringTokenizer(n,"+");
			while(stPlus.hasMoreTokens()){
				String name=stPlus.nextToken().trim();
				if(graph!=null){
					cell=getCellByName(graph,name);
					if(cell!=null){
						reactants.add(cell);
						BiochamEntityData data=(BiochamEntityData)cell.getUserObject();
						sources.add(new BiochamObject(cell.addPort(),data.getMultimerCardinality(),data.getName(),cell));
						data=null;
					}
				}
				name=null;
			}
			stPlus=null;		
		}
		else if(n.contains("-")){
			
			if(graph!=null){
				cell=getCellByName(graph,n.trim());
				if(cell!=null){
					reactants.add(cell);
					BiochamEntityData data=(BiochamEntityData)cell.getUserObject();
					sources.add(new BiochamObject(cell.addPort(),data.getMultimerCardinality(),data.getName(),cell));
					data=null;
				}else{					
				
					//CREATE A COMPLEX(Multimer?) OF REACTANTS..........It already exist, otherwise os complex of products...
					if(collectedReactants!=null){
						
						if(collectedReactants.size()==1){	
							String tp=collectedReactants.get(0);
							if(collectedReactants.get(0).contains("-") && n.contains("-")){
								
								
								cell=getCellByName(graph,n.trim());
								if(cell!=null){
									reactants.add(cell);
									sources.add(new BiochamObject(cell.addPort(),1,n,cell));//data1.getMultimerCardinality()
									graph.getGraphLayoutCache().insert(cell);
								}else{
									
									ArrayList<ContainingMolecule> included=new ArrayList<ContainingMolecule>();		
									
									
									StringTokenizer stk=new StringTokenizer(n,"-");										
									
									
									while(stk.hasMoreTokens()){
									
										String nxtTk="";
										int m=getMultimer(n);										
										if(m>1){
											for(int y=0;y<m;y++){
												nxtTk=stk.nextToken();
											}
										}else{
											nxtTk=stk.nextToken();
										}
										
										
										cell=getCellByName(graph,nxtTk);																				
										
										if(cell!=null){
											
											((BiochamEntityData)cell.getUserObject()).setRepresentingName(nxtTk.trim());
											((BiochamEntityData)cell.getUserObject()).setMoleculeType(getCellType(cell));
											included.add(new ContainingMolecule(((BiochamEntityData)cell.getUserObject()).getName(),((BiochamEntityData)cell.getUserObject()).getMoleculeState(),m,((BiochamEntityData)cell.getUserObject()).isModulator(),getCellType(cell),((BiochamEntityData)cell.getUserObject()).getInitialConcentration(),nxtTk,((BiochamEntityData)cell.getUserObject()).getId(),cell));
										}else{
										
											BiochamEntityData data1=new BiochamEntityData(graph);
											int idx=((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).indexOf(nxtTk.trim());
											if(idx>=0){
												data1.setInitialConcentration(((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).getValue(idx));
											}else{
												data1.setInitialConcentration("0.0");
											}
											data1.setName(nxtTk.trim());
											if(nxtTk.contains("::")){
												String x=nxtTk.substring(nxtTk.lastIndexOf(":")+1);
												data1.setCompartment(x);
												if(nxtTk.substring(0,nxtTk.indexOf(":")).endsWith("}") || nxtTk.substring(0,nxtTk.indexOf(":")).contains("~$")){
													data1.setMoleculeState(MoleculeState.MODIFIED);
													data1.setModificationSites(nxtTk.substring(nxtTk.indexOf("~")+2,nxtTk.indexOf("}")).trim());
												}else{
													data1.setMoleculeState(MoleculeState.NONE);
												}
												x=null;
											}else if(nxtTk.contains("~")){
												data1.setMoleculeState(MoleculeState.MODIFIED);
												data1.setModificationSites(nxtTk.substring(nxtTk.indexOf("~")+2,nxtTk.indexOf("}")).trim());
											}else{
												data1.setMoleculeState(MoleculeState.NONE);
											}						
											data1.setMultimerCardinality(m);
											data1.setRepresentingName(nxtTk);
											data1.setName(n);
											if(n.startsWith("#")){
												cell=new ENucleicAcidFeatureCell(data1);
												data1.setMoleculeType("NucleicAcidFeature");
											}else{
												cell=new EMacromoleculeCell(data1);
												data1.setMoleculeType("Macromolecule");
											}									
											graph.getGraphLayoutCache().insert(cell);
											included.add(new ContainingMolecule(nxtTk,data1.getMoleculeState(),m,false,data1.getMoleculeType(),"0.0",nxtTk,((BiochamEntityData)cell.getUserObject()).getId(),cell));
											data1=null;
										}
										nxtTk=null;
									}
				
									
									
									
									stk=null;
										if(included.size()>1){// || (collectedReactants.size()==1 && ((BiochamEntityData)graph.getCellByName(collectedReactants.get(0).trim()).getUserObject()).getMultimerCardinality()<=1)){
											
											BiochamEntityData data1=new BiochamEntityData(graph);
											int idx=((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).indexOf(n.trim());
											if(idx>=0){
												data1.setInitialConcentration(((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).getValue(idx));
											}else{
												data1.setInitialConcentration("0.0");
											}
											data1.setName(n.trim());
											if(n.contains("::")){
												String x=n.substring(n.lastIndexOf(":")+1);
												data1.setCompartment(x);
												if(n.substring(0,n.indexOf(":")).endsWith("}") || n.substring(0,n.indexOf(":")).contains("~$")){
													data1.setMoleculeState(MoleculeState.MODIFIED);
													data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
												}else{
													data1.setMoleculeState(MoleculeState.NONE);
												}
												x=null;
											}else if(n.contains("~")){
												data1.setMoleculeState(MoleculeState.MODIFIED);
												data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
											}else{
												data1.setMoleculeState(MoleculeState.NONE);
											}									
											
											((BiochamEntityData)data1).setContainingMolecules(included);
											
											cell=new EComplexCell(data1);
											reactants.add(cell);
											sources.add(new BiochamObject(cell.addPort(),1,data1.getName(),cell));//data1.getMultimerCardinality()
											graph.getGraphLayoutCache().insert(cell);
											data1=null;
										}
								//	}
				
									
								}
								
							}else if(collectedReactants.get(0).contains("*")){	
								
								ArrayList<ContainingMolecule> included=new ArrayList<ContainingMolecule>();	
								String molName=collectedReactants.get(0).substring(collectedReactants.get(0).indexOf("*")+1).trim();
								String card=collectedReactants.get(0).substring(0,collectedReactants.get(0).indexOf("*"));		
								
								if(n.contains(molName)){
									
									cell=getCellByName(graph,molName);
									String type=getCellType(cell);							
									
									if(cell!=null){	
									
										BiochamEntityData data1;
										
										if(collectedReactants.size()>1){
											
												data1=new BiochamEntityData(graph);
												data1.setInitialConcentration("0");
												int cardinal=Integer.valueOf(card);
												String nm=molName.trim();
												for(int j=1;j<cardinal;j++){
													nm+="-"+molName.trim();
												}
												data1.setMultimerCardinality(cardinal);												
												data1.setName(nm.trim());
												if(collectedReactants.get(0).contains("::")){
													String x=collectedReactants.get(0).substring(collectedReactants.get(0).lastIndexOf(":")+1);
													data1.setCompartment(x);
													if(collectedReactants.get(0).substring(0,collectedReactants.get(0).indexOf(":")).endsWith("}") || collectedReactants.get(0).substring(0,collectedReactants.get(0).indexOf(":")).contains("~$")){
														data1.setMoleculeState(MoleculeState.MODIFIED);
														data1.setModificationSites(collectedReactants.get(0).substring(collectedReactants.get(0).indexOf("~")+2,collectedReactants.get(0).indexOf("}")).trim());
													}else{
														data1.setMoleculeState(MoleculeState.NONE);
													}
													x=null;
												}else if(collectedReactants.get(0).contains("~")){
													data1.setMoleculeState(MoleculeState.MODIFIED);
													data1.setModificationSites(collectedReactants.get(0).substring(collectedReactants.get(0).indexOf("~")+2,collectedReactants.get(0).indexOf("}")).trim());
												}else{
													data1.setMoleculeState(MoleculeState.NONE);
												}																					
												String nm1=data1.getName();
												if(data1.getMultimerCardinality()>1 && nm1.contains("-")){
													String tmpStr=data1.getName().substring(data1.getName().lastIndexOf("-")+1,data1.getName().length());	
													nm1=data1.getName().substring(0,data1.getName().indexOf(tmpStr))+tmpStr;
													tmpStr=null;
												}
												data1.setRepresentingName(nm1);
												data1.setMoleculeType(type);
												included.add(new ContainingMolecule(data1.getName(),data1.getMoleculeState(),data1.getMultimerCardinality(),data1.isModulator(),type,data1.getInitialConcentration(),nm1,data1.getId(),cell));
											
												nm=null;nm1=null;
												
											}else{
												
												data1=new BiochamEntityData(graph);
												int idx=((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).indexOf(n.trim());
												if(idx>=0){
													data1.setInitialConcentration(((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).getValue(idx));
												}else{
													data1.setInitialConcentration("0.0");
												}
												int cardinal=Integer.valueOf(card);
												String nm=molName.trim();
												for(int j=1;j<cardinal;j++){
													nm+="-"+molName.trim();
												}
												data1.setMultimerCardinality(cardinal);
												data1.setName(n.trim());
												if(n.contains("::")){
													String x=n.substring(n.lastIndexOf(":")+1);
													data1.setCompartment(x);
													if(n.substring(0,n.indexOf(":")).endsWith("}") || n.substring(0,n.indexOf(":")).contains("~$")){
														data1.setMoleculeState(MoleculeState.MODIFIED);
														data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
													}else{
														data1.setMoleculeState(MoleculeState.NONE);
													}
													x=null;
												}else if(n.contains("~")){
													data1.setMoleculeState(MoleculeState.MODIFIED);
													data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
												}else{
													data1.setMoleculeState(MoleculeState.NONE);
												}		
												if(cell instanceof ENucleicAcidFeatureCell){
													cell=new ENucleicAcidFeatureCell(data1);
												}else if(cell instanceof EComplexCell){
													cell=new EComplexCell(data1);
												}else {
													cell=new EMacromoleculeCell(data1);
												}	
												data1.setRepresentingName(molName);												
												reactants.add(cell);
												sources.add(new BiochamObject(cell.addPort(),cardinal,n,cell));//data1.getMultimerCardinality()
												graph.getGraphLayoutCache().insert(cell);
												nm=null;
											}					
											data1=null;
									}
									type=null;
								}							
								molName=null;card=null;included=null;//included.clear();included=null;
							}
						}else{
							
							
							
						//GENERATE THE CONTAINING MOLECULES DATA FROM THE REACTANTS........
						
							ArrayList<ContainingMolecule> included=new ArrayList<ContainingMolecule>();						
						
							for(int i=0;i<collectedReactants.size();i++){							
						
								String molName="";
							
								if(collectedReactants.get(i).contains("*")){
									molName=collectedReactants.get(i).substring(collectedReactants.get(i).indexOf("*")+1).trim();
								}else{
									molName=collectedReactants.get(i).trim();
								}
								
								if(collectedReactants.get(i).contains("*")){							
								
									String card=collectedReactants.get(i).substring(0,collectedReactants.get(i).indexOf("*"));		

									if(n.contains(molName)){
										
										cell=getCellByName(graph,molName);
										String type=getCellType(cell);							
										
										if(cell!=null){	
											
											BiochamEntityData data1;
											
											if(collectedReactants.size()>1){											
											
												data1=new BiochamEntityData(graph);
												data1.setInitialConcentration("0");
												int cardinal=Integer.valueOf(card);
												String nm=molName.trim();
												for(int j=1;j<cardinal;j++){
													nm+="-"+molName.trim();
												}
												data1.setMultimerCardinality(cardinal);												
												data1.setName(nm.trim());
												if(collectedReactants.get(i).contains("::")){
													String x=collectedReactants.get(i).substring(collectedReactants.get(i).lastIndexOf(":")+1);
													data1.setCompartment(x);
													if(collectedReactants.get(i).substring(0,collectedReactants.get(i).indexOf(":")).endsWith("}") || collectedReactants.get(i).substring(0,collectedReactants.get(i).indexOf(":")).contains("~$")){
														data1.setMoleculeState(MoleculeState.MODIFIED);
														data1.setModificationSites(collectedReactants.get(i).substring(collectedReactants.get(i).indexOf("~")+2,collectedReactants.get(i).indexOf("}")).trim());
													}else{
														data1.setMoleculeState(MoleculeState.NONE);
													}
													x=null;
												}else if(collectedReactants.get(i).contains("~")){
													data1.setMoleculeState(MoleculeState.MODIFIED);
													data1.setModificationSites(collectedReactants.get(i).substring(collectedReactants.get(i).indexOf("~")+2,collectedReactants.get(i).indexOf("}")).trim());
												}else{
													data1.setMoleculeState(MoleculeState.NONE);
												}										
												String nm1=data1.getName();
												if(data1.getMultimerCardinality()>1 && nm1.contains("-")){
													String tmpStr=data1.getName().substring(data1.getName().lastIndexOf("-")+1,data1.getName().length());	
													nm1=data1.getName().substring(0,data1.getName().indexOf(tmpStr))+tmpStr;
												}
												data1.setRepresentingName(nm1);
												data1.setMoleculeType(type);
												included.add(new ContainingMolecule(data1.getName(),data1.getMoleculeState(),data1.getMultimerCardinality(),data1.isModulator(),type,data1.getInitialConcentration(),nm1,data1.getId(),cell));
											
												nm=null; nm1=null;
												
											}else{												
												
												data1=new BiochamEntityData(graph);
												int idx=((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).indexOf(n.trim());
												if(idx>=0){
													data1.setInitialConcentration(((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).getValue(idx));
												}else{
													data1.setInitialConcentration("0.0");
												}
												int cardinal=Integer.valueOf(card);
												String nm=molName.trim();
												for(int j=1;j<cardinal;j++){
													nm+="-"+molName.trim();
												}
												data1.setMultimerCardinality(cardinal);												
												data1.setName(n.trim());
												if(n.contains("::")){
													String x=n.substring(n.lastIndexOf(":")+1);
													data1.setCompartment(x);
													if(n.substring(0,n.indexOf(":")).endsWith("}") || n.substring(0,n.indexOf(":")).contains("~$")){
														data1.setMoleculeState(MoleculeState.MODIFIED);
														data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
													}else{
														data1.setMoleculeState(MoleculeState.NONE);
													}
													x=null;
												}else if(n.contains("~")){
													data1.setMoleculeState(MoleculeState.MODIFIED);
													data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
												}else{
													data1.setMoleculeState(MoleculeState.NONE);
												}	
												
												if(cell instanceof ENucleicAcidFeatureCell){
													cell=new ENucleicAcidFeatureCell(data1);
												}else if(cell instanceof EComplexCell){
													cell=new EComplexCell(data1);
												}else {
													cell=new EMacromoleculeCell(data1);
												}		
												reactants.add(cell);
												sources.add(new BiochamObject(cell.addPort(),1,n,cell));//data1.getMultimerCardinality()
												graph.getGraphLayoutCache().insert(cell);
												
												nm=null;												
												break;
											
											}
											data1=null;
										}
										type=null;
									}
									card=null;
								
								}else{
															
									BiochamEntityData data1=new BiochamEntityData(graph);
									data1.setInitialConcentration("0");
									data1.setName(collectedReactants.get(i).trim());
									if(collectedReactants.get(i).contains("::")){
										String x=collectedReactants.get(i).substring(collectedReactants.get(i).lastIndexOf(":")+1);
										data1.setCompartment(x);
										if(collectedReactants.get(i).substring(0,collectedReactants.get(i).indexOf(":")).endsWith("}") || collectedReactants.get(i).substring(0,collectedReactants.get(i).indexOf(":")).contains("~$")){
											data1.setMoleculeState(MoleculeState.MODIFIED);
											data1.setModificationSites(collectedReactants.get(i).substring(collectedReactants.get(i).indexOf("~")+2,collectedReactants.get(i).indexOf("}")).trim());
										}else{
											data1.setMoleculeState(MoleculeState.NONE);
										}
										x=null;
									}else if(collectedReactants.get(i).contains("~")){
										data1.setMoleculeState(MoleculeState.MODIFIED);
										data1.setModificationSites(collectedReactants.get(i).substring(collectedReactants.get(i).indexOf("~")+2,collectedReactants.get(i).indexOf("}")).trim());
									}else{
										data1.setMoleculeState(MoleculeState.NONE);
									}			
									
									//collectedReactants
									String cellName=collectedReactants.get(i).trim();
									cell=getCellByName(graph,cellName.trim());
									String type=getCellType(cell);
									if(cell!=null){									
										String nm1=((BiochamEntityData)cell.getUserObject()).getName();
										if(data1.getMultimerCardinality()>1 && nm1.contains("-")){
											String tmpStr=((BiochamEntityData)cell.getUserObject()).getName().substring(((BiochamEntityData)cell.getUserObject()).getName().lastIndexOf("-")+1,((BiochamEntityData)cell.getUserObject()).getName().length());	
											nm1=((BiochamEntityData)cell.getUserObject()).getName().substring(0,((BiochamEntityData)cell.getUserObject()).getName().indexOf(tmpStr))+tmpStr;
											tmpStr=null;
										}
										((BiochamEntityData)cell.getUserObject()).setRepresentingName(nm1);
										((BiochamEntityData)cell.getUserObject()).setMoleculeType(type);
										included.add(new ContainingMolecule(((BiochamEntityData)cell.getUserObject()).getName(),((BiochamEntityData)cell.getUserObject()).getMoleculeState(),((BiochamEntityData)cell.getUserObject()).getMultimerCardinality(),((BiochamEntityData)cell.getUserObject()).isModulator(),type,((BiochamEntityData)cell.getUserObject()).getInitialConcentration(),nm1,((BiochamEntityData)cell.getUserObject()).getId(),cell));
										nm1=null;
									}
									data1=null;
									cellName=null; type=null;
								}				
								//FINISH for loop....
								molName=null;
							}							
							
						
							//NOW CONSTRUCT THE COMPLEX WITH THE INCLUDED MOLECULES......
							if(collectedReactants.size()>1){// || (collectedReactants.size()==1 && ((BiochamEntityData)graph.getCellByName(collectedReactants.get(0).trim()).getUserObject()).getMultimerCardinality()<=1)){
															
								BiochamEntityData data1=new BiochamEntityData(graph);
								int idx=((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).indexOf(n.trim());
								if(idx>=0){
									data1.setInitialConcentration(((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).getValue(idx));
								}else{
									data1.setInitialConcentration("0.0");
								}
								data1.setName(n.trim());
								if(n.contains("::")){
									String x=n.substring(n.lastIndexOf(":")+1);
									data1.setCompartment(x);
									if(n.substring(0,n.indexOf(":")).endsWith("}") || n.substring(0,n.indexOf(":")).contains("~$")){
										data1.setMoleculeState(MoleculeState.MODIFIED);
										data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
									}else{
										data1.setMoleculeState(MoleculeState.NONE);
									}
									x=null;
								}else if(n.contains("~")){
									data1.setMoleculeState(MoleculeState.MODIFIED);
									data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
								}else{
									data1.setMoleculeState(MoleculeState.NONE);
								}										
								
								((BiochamEntityData)data1).setContainingMolecules(included);
								
								cell=new EComplexCell(data1);
								reactants.add(cell);
								sources.add(new BiochamObject(cell.addPort(),1,data1.getName(),cell));//data1.getMultimerCardinality()
								graph.getGraphLayoutCache().insert(cell);
								
								data1=null;
								
							}
							included=null;
							//included.clear();included=null;
						}
						
						
						
					}else{

						
						if(n.contains("*")){
							
							String cardinality=n.substring(0,n.indexOf("*"));
							
							if(graph!=null){
								
								String nm1=n.substring(n.indexOf("*")+1).trim();
								
								if(nm1.trim().startsWith("(")){
									nm1=nm1.substring(1,nm1.length()-1);
								}
								cell=getCellByName(graph,nm1);
								int card=0;
								try{
									card=Integer.valueOf(cardinality);
								}catch(Exception e){}
								
								if(cell!=null){
									reactants.add(cell);									
									sources.add(new BiochamObject(cell.addPort(),card,nm1.trim(),cell));
								}else{
									
									String name=n.substring(n.indexOf("*")+1).trim();
									BiochamEntityData data1=new BiochamEntityData(graph);
									
									int idx=((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).indexOf(name.trim());
									if(idx>=0){
										data1.setInitialConcentration(((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).getValue(idx));
									}else{
										data1.setInitialConcentration("0.0");
									}
									data1.setName(name);
									if(n.contains("::")){
										String x=n.substring(n.lastIndexOf(":")+1);
										data1.setCompartment(x);
										if(n.substring(0,n.indexOf(":")).endsWith("}") || n.substring(0,n.indexOf(":")).contains("~$")){
											data1.setMoleculeState(MoleculeState.MODIFIED);
											data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
										}else{
											data1.setMoleculeState(MoleculeState.NONE);
										}
										x=null;
									}else if(n.contains("~")){
										data1.setMoleculeState(MoleculeState.MODIFIED);
										data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
									}else{
										data1.setMoleculeState(MoleculeState.NONE);
									}				
									String nm=n.substring(n.indexOf("*")+1).trim();
									if(nm.trim().startsWith("#")){
										cell=new ENucleicAcidFeatureCell(data1);
									}else{
										cell=new EMacromoleculeCell(data1);
									}
									reactants.add(cell);
									sources.add(new BiochamObject(cell.addPort(),card,data1.getName(),cell));
									graph.getGraphLayoutCache().insert(cell);
									
									name=null;
									data1=null;
									nm=null;
								}
								nm1=null;
							}
							cardinality=null;
							
							
						}else if(n.contains("-")){

							StringTokenizer stk=new StringTokenizer(n.trim(),"-");
							ArrayList<ContainingMolecule> included=new ArrayList<ContainingMolecule>();		
							
							while(stk.hasMoreTokens()){
								
								String nxtTk="";
								int m=getMultimer(n);										
								if(m>1){
									for(int y=0;y<m;y++){
										nxtTk=stk.nextToken().trim();
									}
								}else{
									nxtTk=stk.nextToken().trim();
									m=0;
								}
								
								nxtTk=nxtTk.trim();
								cell=getCellByName(graph,nxtTk.trim());																				
								if(cell!=null){
									((BiochamEntityData)cell.getUserObject()).setRepresentingName(nxtTk);
									((BiochamEntityData)cell.getUserObject()).setName(n);
									((BiochamEntityData)cell.getUserObject()).setMoleculeType(getCellType(cell));
									included.add(new ContainingMolecule(((BiochamEntityData)cell.getUserObject()).getName(),((BiochamEntityData)cell.getUserObject()).getMoleculeState(),m,((BiochamEntityData)cell.getUserObject()).isModulator(),getCellType(cell),((BiochamEntityData)cell.getUserObject()).getInitialConcentration(),nxtTk,((BiochamEntityData)cell.getUserObject()).getId(),cell));
								}else{
									
									BiochamEntityData data1=new BiochamEntityData(graph);
									
									int idx=((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).indexOf(nxtTk.trim());
									if(idx>=0){
										data1.setInitialConcentration(((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).getValue(idx));
									}else{
										data1.setInitialConcentration("0.0");
									}
									if(m>1){
										data1.setName(n.trim());	
									}else{
										data1.setName(nxtTk.trim());
									}
									
									if(nxtTk.contains("::")){
										String x=nxtTk.substring(nxtTk.lastIndexOf(":")+1);
										data1.setCompartment(x);
										if(nxtTk.substring(0,nxtTk.indexOf(":")).endsWith("}") || nxtTk.substring(0,nxtTk.indexOf(":")).contains("~$")){
											data1.setMoleculeState(MoleculeState.MODIFIED);
											data1.setModificationSites(nxtTk.substring(nxtTk.indexOf("~")+2,nxtTk.indexOf("}")).trim());
										}else{
											data1.setMoleculeState(MoleculeState.NONE);
										}
										x=null;
									}else if(nxtTk.contains("~")){
										data1.setMoleculeState(MoleculeState.MODIFIED);
										data1.setModificationSites(nxtTk.substring(nxtTk.indexOf("~")+2,nxtTk.indexOf("}")).trim());
									}else{
										data1.setMoleculeState(MoleculeState.NONE);
									}							
									
									
									data1.setRepresentingName(nxtTk);									
									data1.setMultimerCardinality(m);
									if(nxtTk.startsWith("#")){
									
										data1.setMoleculeType("NucleicAcidFeature");
										cell=new ENucleicAcidFeatureCell(data1);
									}else{
										
										data1.setMoleculeType("Macromolecule");
										cell=new EMacromoleculeCell(data1);
									}			
									graph.getGraphLayoutCache().insert(cell);
									included.add(new ContainingMolecule(n,data1.getMoleculeState(),m,false,data1.getMoleculeType(),"0.0",nxtTk,((BiochamEntityData)cell.getUserObject()).getId(),cell));
									
									//data1=null;
								}
								nxtTk=null;
							}
							if(included.size()>1){// || (collectedReactants.size()==1 && ((BiochamEntityData)graph.getCellByName(collectedReactants.get(0).trim()).getUserObject()).getMultimerCardinality()<=1)){
								
								BiochamEntityData data1=new BiochamEntityData(graph);
								
								int idx=((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).indexOf(n.trim());
								if(idx>=0){
									data1.setInitialConcentration(((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).getValue(idx));
								}else{
									data1.setInitialConcentration("0.0");
								}
								data1.setName(n.trim());
								if(n.contains("::")){
									String x=n.substring(n.lastIndexOf(":")+1);
									data1.setCompartment(x);
									if(n.substring(0,n.indexOf(":")).endsWith("}") || n.substring(0,n.indexOf(":")).contains("~$")){
										data1.setMoleculeState(MoleculeState.MODIFIED);
										data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
									}else{
										data1.setMoleculeState(MoleculeState.NONE);
									}
									x=null;
								}else if(n.contains("~")){
									data1.setMoleculeState(MoleculeState.MODIFIED);
									data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
								}else{
									data1.setMoleculeState(MoleculeState.NONE);
								}											
								
								((BiochamEntityData)data1).setContainingMolecules(included);
								
								cell=new EComplexCell(data1);
								reactants.add(cell);
								sources.add(new BiochamObject(cell.addPort(),1,data1.getName(),cell));//data1.getMultimerCardinality()
								graph.getGraphLayoutCache().insert(cell);
								
								data1=null;
							}else{	
								
								reactants.add(included.get(0).getInstance());
								sources.add(new BiochamObject(included.get(0).getInstance().addPort(),included.get(0).getCardinality(),included.get(0).getName(),included.get(0).getInstance()));//data1.getMultimerCardinality()
							}							
						
							stk=null;included=null;
							//included.clear();included=null;
						}
						else{	
							
							
							
							
							if(graph!=null){
								
								cell=getCellByName(graph,n.trim());
								if(cell!=null){
									reactants.add(cell);
									BiochamEntityData data=(BiochamEntityData)cell.getUserObject();
									sources.add(new BiochamObject(cell.addPort(),data.getMultimerCardinality(),data.getName(),cell));
									data=null;
								}else{

									BiochamEntityData data1=new BiochamEntityData(graph);
									
									int idx=((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).indexOf(n.trim());
									if(idx>=0){
										data1.setInitialConcentration(((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).getValue(idx));
									}else{
										data1.setInitialConcentration("0.0");
									}
									data1.setName(n.trim());
									if(n.contains("::")){
										String x=n.substring(n.lastIndexOf(":")+1);
										data1.setCompartment(x);
										if(n.substring(0,n.indexOf(":")).endsWith("}") || n.substring(0,n.indexOf(":")).contains("~$")){
											data1.setMoleculeState(MoleculeState.MODIFIED);
											data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
										}else{
											data1.setMoleculeState(MoleculeState.NONE);
										}
										x=null;
									}else if(n.contains("~")){
										data1.setMoleculeState(MoleculeState.MODIFIED);
										data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
									}else{
										data1.setMoleculeState(MoleculeState.NONE);
									}											
									if(n.startsWith("#")){
										cell=new ENucleicAcidFeatureCell(data1);
										data1.setMoleculeType("NucleicAcidFeature");
									}else{
										cell=new EMacromoleculeCell(data1);
										data1.setMoleculeType("Macromolecule");
									}		
									reactants.add(cell);
									sources.add(new BiochamObject(cell.addPort(),data1.getMultimerCardinality(),data1.getName(),cell));
									graph.getGraphLayoutCache().insert(cell);
									
									data1=null;
								}
							}
						}					
					}
				}
			}
			
		
		//Is it a simple or multiplied MOLECULE????????????
		}else{
			
			
						
			if(n.contains("*")){
				
				String cardinality=n.substring(0,n.indexOf("*"));
				
				if(graph!=null){
				
					cell=getCellByName(graph,n.substring(n.indexOf("*")+1).trim());
					int card=0;
					try{
						card=Integer.valueOf(cardinality);
					}catch(Exception e){}
					
					if(cell!=null){
						reactants.add(cell);
						sources.add(new BiochamObject(cell.addPort(),card,n.substring(n.indexOf("*")+1).trim(),cell));
					}else{

						BiochamEntityData data1=new BiochamEntityData(graph);						
						String name=n.substring(n.indexOf("*")+1).trim();
						
						int idx=((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).indexOf(name.trim());
						if(idx>=0){
							data1.setInitialConcentration(((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).getValue(idx));
						}else{
							data1.setInitialConcentration("0.0");
						}
						data1.setName(name);
						if(n.contains("::")){
							String x=n.substring(n.lastIndexOf(":")+1);
							data1.setCompartment(x);
							if(n.substring(0,n.indexOf(":")).endsWith("}") || n.substring(0,n.indexOf(":")).contains("~$")){
								data1.setMoleculeState(MoleculeState.MODIFIED);
								data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
							}else{
								data1.setMoleculeState(MoleculeState.NONE);
							}
							x=null;
						}else if(n.contains("~")){
							data1.setMoleculeState(MoleculeState.MODIFIED);
							data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
						}else{
							data1.setMoleculeState(MoleculeState.NONE);
						}			
						
						String nm=n.substring(n.indexOf("*")+1).trim();
						if(nm.trim().startsWith("#")){
							cell=new ENucleicAcidFeatureCell(data1);
						}else{
							cell=new EMacromoleculeCell(data1);
						}
						reactants.add(cell);
						sources.add(new BiochamObject(cell.addPort(),card,data1.getName(),cell));
						graph.getGraphLayoutCache().insert(cell);
						
						data1=null;
						name=null;
						nm=null;
					}
				}
				cardinality=null;
			}else{	
								
				if(graph!=null){
				
					cell=getCellByName(graph,n.trim());
					if(cell!=null){
						reactants.add(cell);
						BiochamEntityData data=(BiochamEntityData)cell.getUserObject();
						sources.add(new BiochamObject(cell.addPort(),data.getMultimerCardinality(),data.getName(),cell));
						data=null;
					}else{

						BiochamEntityData data1=new BiochamEntityData(graph);
						
						int idx=((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).indexOf(n.trim());
						if(idx>=0){
							data1.setInitialConcentration(((ParamTableInitConc)graph.getBiochamModel().getInitConditions().getParamTable()).getValue(idx));
						}else{
							data1.setInitialConcentration("0.0");
						}
						data1.setName(n.trim());
						if(n.contains("::")){
							String x=n.substring(n.lastIndexOf(":")+1);
							data1.setCompartment(x);
							if(n.substring(0,n.indexOf(":")).endsWith("}") || n.substring(0,n.indexOf(":")).contains("~$")){
								data1.setMoleculeState(MoleculeState.MODIFIED);
								data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
							}else{
								data1.setMoleculeState(MoleculeState.NONE);
							}
							/*if(n.contains("~{")){
								data1.setCompartment(n.substring(n.lastIndexOf(":"),n.indexOf("~")));
							}else{
								data1.setCompartment(n.substring(n.lastIndexOf(":")));								
							}*/
							x=null;
						}else if(n.contains("~")){
							data1.setMoleculeState(MoleculeState.MODIFIED);
							data1.setModificationSites(n.substring(n.indexOf("~")+2,n.indexOf("}")).trim());
						}else{
							data1.setMoleculeState(MoleculeState.NONE);
						}										
						if(n.startsWith("#")){
							cell=new ENucleicAcidFeatureCell(data1);
							data1.setMoleculeType("NucleicAcidFeature");
						}else{
							cell=new EMacromoleculeCell(data1);
							data1.setMoleculeType("Macromolecule");
						}		
						reactants.add(cell);
						sources.add(new BiochamObject(cell.addPort(),data1.getMultimerCardinality(),data1.getName(),cell));
						graph.getGraphLayoutCache().insert(cell);
						
						data1=null;
					}
				}
			}
		}
		cell=null;
	}
	public static int getMultimer(String n){
		StringTokenizer st=new StringTokenizer(n.trim(),"-");
		String tk1="",tk2="";
		int cnt=1;
		if(st.countTokens()>0){
			tk1=st.nextToken().trim();
		}
		while(st.hasMoreTokens()){
			tk2=st.nextToken().trim();
			if(tk1.trim().equals(tk2.trim())){
				cnt++;
			}else{
				break;
			}
		}
		st=null;
		tk1=null; tk2=null;
		return cnt;		
	}

	public static String getNucleicName(String nm){
		String tp="";
		if(!nm.trim().startsWith("#")){
			tp="#"+nm;
			nm="";
			nm=tp;
		}
		tp=null;
		return nm;
		
	}
	public static BiochamEdgeData setReactants(BiochamEdgeData userObject, ArrayList<BiochamObject> sources) {
		
		ArrayList<BiochamEnitity> rs=new ArrayList<BiochamEnitity>(sources.size());
		for(int i=0;i<sources.size();i++){				
			rs.add(new BiochamEnitity(((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getId(),((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getName()));
		}
		userObject.setReactants(rs);
		return userObject;
	}
	public static BiochamEdgeData setReactantsFromCells(BiochamEdgeData userObject, ArrayList<DefaultGraphCell> sources) {
		
		ArrayList<BiochamEnitity> rs=new ArrayList<BiochamEnitity>(sources.size());
		for(int i=0;i<sources.size();i++){				
			rs.add(new BiochamEnitity(((BiochamEntityData)sources.get(i).getUserObject()).getId(),((BiochamEntityData)sources.get(i).getUserObject()).getName()));
		}
		userObject.setReactants(rs);
		return userObject;
	}
	public static BiochamEdgeData setProducts(BiochamEdgeData userObject, ArrayList<BiochamObject> sources) {
		
		ArrayList<BiochamEnitity> rs=new ArrayList<BiochamEnitity>(sources.size());
		for(int i=0;i<sources.size();i++){				
			rs.add(new BiochamEnitity(((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getId(),((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getName()));
		}
		userObject.setProducts(rs);
		return userObject;
	}
	public static BiochamEdgeData setProductsFromCells(BiochamEdgeData userObject, ArrayList<DefaultGraphCell> sources) {
		
		ArrayList<BiochamEnitity> rs=new ArrayList<BiochamEnitity>(sources.size());
		for(int i=0;i<sources.size();i++){				
			rs.add(new BiochamEnitity(((BiochamEntityData)sources.get(i).getUserObject()).getId(),((BiochamEntityData)sources.get(i).getUserObject()).getName()));
		}
		userObject.setProducts(rs);
		return userObject;
	}	
	public static BiochamEdgeData setModulators1(BiochamEdgeData userObject, ArrayList<BiochamObject> sources) {
		
		ArrayList<BiochamEnitity> rs=new ArrayList<BiochamEnitity>(sources.size());
		for(int i=0;i<sources.size();i++){				
			rs.add(new BiochamEnitity(((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getId(),((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getName()));
		}
		userObject.setModulators1(rs);
		return userObject;
	}
	public static BiochamEdgeData setModulators1FromCells(BiochamEdgeData userObject, ArrayList<DefaultGraphCell> sources) {
		
		ArrayList<BiochamEnitity> rs=new ArrayList<BiochamEnitity>(sources.size());
		for(int i=0;i<sources.size();i++){				
			rs.add(new BiochamEnitity(((BiochamEntityData)sources.get(i).getUserObject()).getId(),((BiochamEntityData)sources.get(i).getUserObject()).getName()));
		}
		userObject.setModulators1(rs);
		return userObject;
	}
	public static BiochamEdgeData setModulators2FromCells(BiochamEdgeData userObject, ArrayList<DefaultGraphCell> sources) {
		
		ArrayList<BiochamEnitity> rs=new ArrayList<BiochamEnitity>(sources.size());
		for(int i=0;i<sources.size();i++){				
			rs.add(new BiochamEnitity(((BiochamEntityData)sources.get(i).getUserObject()).getId(),((BiochamEntityData)sources.get(i).getUserObject()).getName()));
		}
		userObject.setModulators2(rs);
		return userObject;
	}
	public static BiochamEdgeData setModulators2(BiochamEdgeData userObject, ArrayList<BiochamObject> sources) {
		
		ArrayList<BiochamEnitity> rs=new ArrayList<BiochamEnitity>(sources.size());
		for(int i=0;i<sources.size();i++){				
			rs.add(new BiochamEnitity(((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getId(),((BiochamEntityData)sources.get(i).getInstance().getUserObject()).getName()));
		}
		userObject.setModulators2(rs);
		return userObject;
	}
	/*public static BiochamEdgeData setModulators2(BiochamEdgeData userObject, ArrayList<DefaultGraphCell> modulators) {
		ArrayList<BiochamEnitity> rs=new ArrayList<BiochamEnitity>(modulators.size());
		for(int i=0;i<modulators.size();i++){				
			rs.add(new BiochamEnitity(((BiochamEntityData)modulators.get(i).getUserObject()).getId(),((BiochamEntityData)modulators.get(i).getUserObject()).getName()));
		}
		userObject.setModulators2(rs);
		return userObject;
	}*/
	
	
	public static void addNewRuleToContainingRules(BiochamGraph graph, String ruleName, Object[] ruleObjects){
		graph.getContainingRules().put(ruleName, ruleObjects);
	}
	public static void deleteRuleFromContainingRules(BiochamGraph graph, String ruleName){
		graph.getContainingRules().remove(ruleName);
	}
	public static void modifyRuleFromContainingRules(BiochamGraph graph, String oldRN, String newRN, Object[] ruleObjects){
		Object[] newValue=null;
		if(graph.getContainingRules().get(oldRN)!=null){
			newValue=new Object[ruleObjects.length+graph.getContainingRules().get(oldRN).length];
			int i;
			Object[] otherValues=graph.getContainingRules().get(oldRN);
			for(i=0;i<ruleObjects.length;i++){
				Utils.debugMsg(ruleObjects[i].getClass().toString());
				newValue[i]=ruleObjects[i];
			}
			for(int k=i,j=0;j<otherValues.length;k++,j++){
				Utils.debugMsg(otherValues[j].getClass().toString());
				newValue[k]=otherValues[j];
			}
		}
		if(newValue!=null){
			graph.getContainingRules().remove(oldRN);
			graph.getContainingRules().put(newRN, newValue);	
		}
		
	}
	
	public static String addKineticsToBiochamRule(String rule,String reactant) {
		
		String newName=null;
		String leftSide, elseSide;
		if(rule.contains("<=")){
			leftSide=rule.substring(0,rule.indexOf("<"));
			elseSide=rule.substring(rule.indexOf("<")-1);
		}else{
			leftSide=rule.substring(0,rule.indexOf("="));
			elseSide=rule.substring(rule.indexOf("="));
		}
		
		newName=leftSide+"+"+reactant+elseSide;
				
		return newName;
		
	}
	public static String addReactantToBiochamRule(String rule,String reactant) {
		if(reactant.contains("(")){
			reactant=reactant.substring(0,reactant.lastIndexOf("("));
		}
		String newName=null;
		String leftSide, elseSide;
		if(rule.contains("<=")){
			leftSide=rule.substring(0,rule.indexOf("<"));
			elseSide=rule.substring(rule.indexOf("<")-1);
		}else{
			leftSide=rule.substring(0,rule.indexOf("="));
			elseSide=rule.substring(rule.indexOf("="));
		}
		
		newName=leftSide+"+"+reactant+elseSide;
				
		return newName;
		
	}
	
	public static String addProductToBiochamRule(String rule, String product) {
		if(product.contains("(")){
			product=product.substring(0,product.lastIndexOf("("));
		}
		return rule+"+"+product;
	}
	
	public static String addModulatorToBiochamRule(String rule, String modulator) {
		if(modulator.contains("(")){
			modulator=modulator.substring(0,modulator.lastIndexOf("("));
		}
		String newName=null;
		String leftSide, elseSide;
		if(rule.contains("<=")){
			leftSide=rule.substring(0,rule.indexOf("<"));
			elseSide=rule.substring(rule.indexOf("<"));
		}else{
			leftSide=rule.substring(0,rule.indexOf("="));
			elseSide=rule.substring(rule.indexOf("="));
		}
		
		newName=leftSide+"+"+modulator+elseSide+"+"+modulator;
				
		return newName;
	}
	public static void addObjectsToReactionContents(BiochamGraph graph, String rule, Object[] newObjects) {
		Object[] newValue=null;
		if(graph.getContainingRules().get(rule)!=null){
			newValue=new Object[newObjects.length+graph.getContainingRules().get(rule).length];
			int i;
			Object[] otherValues=graph.getContainingRules().get(rule);
			for(i=0;i<newObjects.length;i++){
				//System.out.println(newObjects[i].getClass().toString());
				newValue[i]=newObjects[i];
			}
			for(int k=i,j=0;j<otherValues.length;k++,j++){
				//System.out.println(otherValues[j].getClass().toString());
				newValue[k]=otherValues[j];
			}
		}
		if(newValue!=null){
			//graph.getContainingRules().remove(oldRN);
			graph.getContainingRules().put(rule, newValue);	
		}
		
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
}
