package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultGraphCell;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.utils.Utils;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;

import javax.swing.JOptionPane;

public class DotToSBGNGraph {

	
	
	public static void read(File f,BiochamGraph graph,boolean layoutFromDOT){	
		//read line by line
		
		//catch the labels and shape content
		//if its a molecule, search if it exists on the graph
		//if it exists, place it,
		//otherwise, nothing.....
		
		//catch if the line contains the sym "->",
		//if yes, it is a reaction
		//depending of the labels(left/right), 
		//its a type of the 5 sbgn reactions?
		// A LITTLE MORE COMPLICATED!!!
		
		//position of reactions according to its labels(rectSt,circleAssoc,circleDissoc)' position.
				
		try {
		
			
			 
			
			 BufferedReader in=new BufferedReader(new FileReader(f));
			 String line;
			 
			 HashMap<String,String> positions=new HashMap<String,String>();			
			 ArrayList<String> molecules=new ArrayList<String>();			
			 ArrayList<String> stateTransitions=new ArrayList<String>();
			 ArrayList<String> associations=new ArrayList<String>();
			 ArrayList<String> dissociations=new ArrayList<String>();
			 ArrayList<String> invisibles=new ArrayList<String>();
			 ArrayList<String> zeros=new ArrayList<String>();
			 DotObject d;
			 DotReaction dr;		
			 ArrayList<String> reactants=new ArrayList<String>();
			 ArrayList<String> products=new ArrayList<String>();
			 ArrayList<String> modulators=new ArrayList<String>();
			 String savedLine=null;
			 ArrayList<DotObject> sbgnMols=new ArrayList<DotObject>();
			 ArrayList<DotReaction> sbgnReactions=new ArrayList<DotReaction>();
			 while(true){
				 //(line=in.readLine().trim())!=null){				 
				 if(savedLine!=null){
					 line=savedLine;
					 savedLine=null;
				 }else{
					 line=in.readLine();					 
				 }
				 if(line!=null){
					 line=line.trim();
					 if(line.contains("[label=") && !(line.contains("N") || line.contains("node"))){ //catch up the graph objects
						 
						 //put positions of every object
						 String t1=line.substring(0,line.indexOf("[")).trim();
						/* int in1=line.indexOf("pos=")+4;
						 int in2=0;
						 if(line.substring(line.indexOf("pos=")+10).contains(",")){
							 in2=in1+6+line.substring(line.indexOf("pos=")+10).indexOf(",");	 
						 }else{
							 in2=line.lastIndexOf("]");
						 }
						 */
						 String t2=line.substring(line.indexOf("pos")+5,line.lastIndexOf("]")-1);
						 if(t1.startsWith("\"")){
							 t1=t1.substring(1);
						 }
						 if(t1.endsWith("\"")){
							 t1=t1.substring(0,t1.length()-1);
						 }	
						 positions.put(t1,t2);
						 Utils.debugMsg(t1+"=>"+t2);
						 //put objects in suitable container
						 if(line.contains("shape=circle") && line.contains("filled") && line.contains("black")){
							 associations.add(t1);
						 }else if(line.contains("O") && line.contains("circle")){
							 dissociations.add(t1);
						 }else if(line.contains("label=\"\"") && line.contains("shape=rect")){
							 stateTransitions.add(t1);
						 }else if(line.contains("style=invis")){
							 invisibles.add(t1);
						 }else if(line.contains("shape=circle") && line.contains("/")){
							 zeros.add(t1);
						 }else{
							 molecules.add(t1);
							 d=new DotObject();	
							 d.setName(t1);
							 String c=line.substring(line.indexOf("fillcolor=")+11,line.indexOf("fillcolor=")+11+7);
							 d.setColor(c);
							 String w=line.substring(line.indexOf("width=")+5,line.indexOf("width=")+5+line.substring(line.indexOf("width=")+5).indexOf(","));
							 d.setWidth(w);
							 String h=line.substring(line.trim().indexOf("height=")+5,line.trim().indexOf("height=")+5+line.substring(line.trim().indexOf("height=")+5).indexOf(","));
							 d.setHeight(h);
							 String t=line.substring(line.indexOf("shape=")+6,line.indexOf("shape=")+6+line.substring(line.indexOf("shape=")+6).indexOf(","));
							 if(t.contains("octagon")){
								 d.setSbgnType("Complex");
							 }else if(t.contains("box")){
								 d.setSbgnType("Macromolecule");
							 }else{
								 d.setSbgnType("Gene");
							 }
							 d.setPosition(t2);
							 sbgnMols.add(d);
						 }
						 
					 }else if(line.contains("->")){ //parse the graph reactions......
						 //(how the graph objects are related to each other)
						 
						 String left=line.substring(0,line.indexOf("->")).trim();
						 if(left.startsWith("\"")){
							 left=left.substring(1);
						 }
						 if(left.endsWith("\"")){
							 left=left.substring(0,left.length()-1);
						 }	
						 String right;
						 if(line.substring(line.indexOf("->")).contains("[")){
							 right=line.substring(line.indexOf("->")+2,line.indexOf("[")).trim();						 
						 }else{
							 right=line.substring(line.indexOf("->")+2).trim();
						 }			
						 if(right.startsWith("\"")){
							 right=right.substring(1);
						 }
						 if(right.endsWith("\"")){
							 right=right.substring(0,right.length()-1);
						 }	
						 if(invisibles.contains(right)){
							 //its a beginning of st or assoc
							
							 reactants.add(left);
							 String next;
							 while((next=in.readLine().trim()).contains(right)){
								 String tmpL=next.substring(0,next.indexOf("->")).trim();
								 if(tmpL.startsWith("\"")){
									 tmpL=tmpL.substring(1);
								 }
								 if(tmpL.endsWith("\"")){
									 tmpL=tmpL.substring(0,tmpL.length()-1);
								 }	
								 String tmpR;
								 if(next.contains("[")){
									 tmpR=next.substring(next.indexOf("->")+2,next.indexOf("[")).trim();
								 }else{
									 tmpR=next.substring(next.indexOf("->")+2).trim();
								 }
								 if(tmpR.startsWith("\"")){
									 tmpR=tmpR.substring(1);
								 }
								 if(tmpR.endsWith("\"")){
									 tmpR=tmpR.substring(0,tmpR.length()-1);
								 }			
								 				 	
								 if(tmpR.equals(right)){
									 reactants.add(tmpL);
								 }else{
									 if(stateTransitions.contains(tmpR)){
										 //its a state transition										
										 dr=new DotReaction();
										 dr.setSbgnType("State Transition");
										 Utils.debugMsg("STATE_TRANSITION");
										 dr.setReactionGlyph(tmpR);
										 dr.setLeftPosition(positions.get(right));
										// String pos=line.substring(line.indexOf("pos")+4,line.lastIndexOf("]"));
										 dr.setMiddlePosition(positions.get(tmpR));
										 dr.initiateElements();
										 String nx=in.readLine().trim();
										 String inv=nx.substring(nx.indexOf("->")+2,nx.indexOf("[")).trim();
										 if(inv.startsWith("\"")){
											 inv=inv.substring(1);
										 }
										 if(inv.endsWith("\"")){
											 inv=inv.substring(0,inv.length()-1);
										 }	
										 dr.setRightPosition(positions.get(inv));
										 String nx2;
										 while((nx2=in.readLine().trim()).contains(inv)){
											 if(nx2.contains("[")){
												 String tp=nx2.substring(nx2.indexOf("->")+2,nx2.indexOf("[")).trim();
												 if(tp.startsWith("\"")){
													 tp=tp.substring(1);
												 }
												 if(tp.endsWith("\"")){
													 tp=tp.substring(0,tp.length()-1);
												 }	
												 products.add(tp);
											 }else{
												 String tp=nx2.substring(nx2.indexOf("->")+2).trim();
												 if(tp.startsWith("\"")){
													 tp=tp.substring(1);
												 }
												 if(tp.endsWith("\"")){
													 tp=tp.substring(0,tp.length()-1);
												 }
												 products.add(tp);
											 }
											 
										 }										
										 dr.setReactants(new ArrayList<String>(reactants));
										 dr.setProducts(new ArrayList<String>(products));
										 savedLine=nx2;
										// System.out.println(dr.getReactants().size()+","+dr.getProducts().size()+","+dr.getModulators().size());
										 if(savedLine.contains(tmpR)){
											 //modulators
											 String mod=savedLine.substring(0,savedLine.indexOf("->")).trim();
											 if(mod.startsWith("\"")){
												 mod=mod.substring(1);
											 }
											 if(mod.endsWith("\"")){
												 mod=mod.substring(0,mod.length()-1);
											 }	
											 dr.getModulators().add(mod);		
										 }else{
											 sbgnReactions.add(dr);
											 reactants.clear();
											 products.clear();
											 modulators.clear();
											// System.out.println(dr.getReactants().size()+","+dr.getProducts().size()+","+dr.getModulators().size());
											 break;
										 }
										 while((savedLine=in.readLine().trim()).contains(tmpR)){
											 String mod=savedLine.substring(0,savedLine.indexOf("->")).trim();
											 if(mod.startsWith("\"")){
												 mod=mod.substring(1);
											 }
											 if(mod.endsWith("\"")){
												 mod=mod.substring(0,mod.length()-1);
											 }	
											 dr.getModulators().add(mod);	
										 }
										 sbgnReactions.add(dr);
										 //System.out.println(dr.getReactants().size()+","+dr.getProducts().size()+","+dr.getModulators().size());
										 reactants.clear();
										 products.clear();
										 modulators.clear();
										// System.out.println(dr.getReactants().size()+","+dr.getProducts().size()+","+dr.getModulators().size());
										 break;
									 }else if(associations.contains(tmpR)){
										 //its an association
										 dr=new DotReaction();
										 dr.setReactionGlyph(tmpR);
										 dr.setSbgnType("Association");
										 Utils.debugMsg("ASSOCIATION");
										 dr.setLeftPosition(positions.get(right));
										 dr.setMiddlePosition(positions.get(tmpR));
										 dr.initiateElements();
										 String nx=in.readLine().trim();
										// System.out.println(dr.getReactants().size()+","+dr.getProducts().size()+","+dr.getModulators().size());
										 String complex;
										 if(nx.contains("[")){
											 complex=nx.substring(nx.indexOf("->")+2,nx.lastIndexOf("[")).trim();
										 }else{
											 complex=nx.substring(nx.indexOf("->")+2).trim();
										 }
										 if(complex.startsWith("\"")){
											 complex=complex.substring(1);
										 }
										 if(complex.endsWith("\"")){
											 complex=complex.substring(0,complex.length()-1);
										 }					 
										 dr.getProducts().add(complex);
										 dr.setReactants(new ArrayList<String>(reactants));																												 
										 savedLine=in.readLine().trim();
										// System.out.println(dr.getReactants().size()+","+dr.getProducts().size()+","+dr.getModulators().size());
										 if(savedLine.contains(tmpR)){
											 //modulators
											 String mod=savedLine.substring(0,savedLine.indexOf("->")).trim();
											 if(mod.startsWith("\"")){
												 mod=mod.substring(1);
											 }
											 if(mod.endsWith("\"")){
												 mod=mod.substring(0,mod.length()-1);
											 }	
											 dr.getModulators().add(mod);		
										 }else{
											 //System.out.println(dr.getReactants().size()+","+dr.getProducts().size()+","+dr.getModulators().size());
											 reactants.clear();
											 products.clear();
											 modulators.clear();
											 sbgnReactions.add(dr);
											// System.out.println(dr.getReactants().size()+","+dr.getProducts().size()+","+dr.getModulators().size());
											 break;
										 }
										 while((savedLine=in.readLine().trim()).contains(tmpR)){	
											 String mod=savedLine.substring(0,savedLine.indexOf("->")).trim();
											 if(mod.startsWith("\"")){
												 mod=mod.substring(1);
											 }
											 if(mod.endsWith("\"")){
												 mod=mod.substring(0,mod.length()-1);
											 }	
											 dr.getModulators().add(mod);	
										 }
										// System.out.println(dr.getReactants().size()+","+dr.getProducts().size()+","+dr.getModulators().size());
										 sbgnReactions.add(dr);
										 reactants.clear();
										 products.clear();
										 modulators.clear();
										// System.out.println(dr.getReactants().size()+","+dr.getProducts().size()+","+dr.getModulators().size());
										 break;
									 }
								 }
							 }
							 
						 }else if(dissociations.contains(right)){
							 //its a beginning of a dissoc
							 dr=new DotReaction();
							 dr.setSbgnType("Dissociation");
							 Utils.debugMsg("DISSOCIATION");
							 //String pos=line.substring(line.indexOf("pos")+4,line.lastIndexOf("]"));
							 dr.setMiddlePosition(positions.get(right));
							 dr.initiateElements();
							 dr.getReactants().add(left);
							 //dr.setReactants(new ArrayList<String>(reactants));											 
							 String nx=in.readLine().trim();
							 String inv=nx.substring(nx.indexOf("->")+2,nx.indexOf("[")).trim();	
							 if(inv.startsWith("\"")){
								 inv=inv.substring(1);
							 }
							 if(inv.endsWith("\"")){
								 inv=inv.substring(0,inv.length()-1);
							 }	
							 dr.setRightPosition(positions.get(inv));
							 String next;
							// System.out.println(dr.getReactants().size()+","+dr.getProducts().size()+","+dr.getModulators().size());
							 while((next=in.readLine().trim()).contains(inv)){
								 String tp;
								 if(next.contains("[")){
									 tp=next.substring(next.indexOf("->")+2,next.indexOf("[")).trim();
								 }else{
									 tp=next.substring(next.indexOf("->")+2).trim();
								 }
								 if(tp.startsWith("\"")){
									 tp=tp.substring(1);
								 }
								 if(tp.endsWith("\"")){
									 tp=tp.substring(0,tp.length()-1);
								 }							 
								 dr.getProducts().add(tp);
							 }
							 if(next.contains(right)){
								 //modulators
								 String mod=next.substring(0,next.indexOf("->")).trim();
								 if(mod.startsWith("\"")){
									 mod=mod.substring(1);
								 }
								 if(mod.endsWith("\"")){
									 mod=mod.substring(0,mod.length()-1);
								 }	
								 dr.getModulators().add(mod);								 
							 }else{
								 //System.out.println(dr.getReactants().size()+","+dr.getProducts().size()+","+dr.getModulators().size());
								 savedLine=next;
								 sbgnReactions.add(dr);
								 reactants.clear();
								 products.clear();
								 modulators.clear();
								// System.out.println(dr.getReactants().size()+","+dr.getProducts().size()+","+dr.getModulators().size());
								 continue;
							 }
							// System.out.println(dr.getReactants().size()+","+dr.getProducts().size()+","+dr.getModulators().size());
							 while((savedLine=in.readLine().trim()).contains(right)){	
								 String mod=savedLine.substring(0,savedLine.indexOf("->")).trim();
								 if(mod.startsWith("\"")){
									 mod=mod.substring(1);
								 }
								 if(mod.endsWith("\"")){
									 mod=mod.substring(0,mod.length()-1);
								 }	
								 dr.getModulators().add(mod);	
							 }
							 //System.out.println(dr.getReactants().size()+","+dr.getProducts().size()+","+dr.getModulators().size());
							 sbgnReactions.add(dr);
							 reactants.clear();
							 products.clear();
							 modulators.clear();
							// System.out.println(dr.getReactants().size()+","+dr.getProducts().size()+","+dr.getModulators().size());
						 } 
					 }
				 }else{
					 break;
				 }
			 }
			 
			 
			 DefaultGraphCell[] reactions=GraphUtilities.getAllReactions(graph);
			 
			 
			/* System.out.println("\n\n***************RECAPITULIF*****************");
			 System.out.println("_______________________________________________ ");
			 System.out.println("Graph: reactions="+ reactions.length+", molecules="+GraphUtilities.getAllMoleculeCells(graph).length);
			 System.out.println("DOT File: reactions="+ sbgnReactions.size()+", molecules="+(sbgnMols.size()+zeros.size()));
			 System.out.println("_______________________________________________ ");
			 System.out.println("\n\n*******************************************");*/
			 
			 BiochamEdgeData dt;			 		 
			 for(int i=0;i<reactions.length;i++){//for all the reactions of the graph
				 
				 dt=(BiochamEdgeData)reactions[i].getUserObject();
				// System.out.println("GRAPH: Searching for reaction="+ dt.getName());
				 if(dt!=null){
					 for(int j=0;j<sbgnReactions.size();j++){//for each reaction of the DOT file..............
						// System.out.println("DOT File: Searching for reaction="+ sbgnReactions.get(j).sbgnType+",reactants:"+sbgnReactions.get(j).getReactants().size()+",products:"+sbgnReactions.get(j).getProducts().size()+",mods:"+sbgnReactions.get(j).getModulators().size());
						 //find if there are some mathcing reactions of the given dot layout.....
						/*for(int q=0;q<sbgnReactions.get(j).getReactants().size();q++){
							//System.out.println("DOT r: "+sbgnReactions.get(j).getReactants().get(q));
						}
						for(int q=0;q<sbgnReactions.get(j).getProducts().size();q++){
							//System.out.println("DOT p: "+sbgnReactions.get(j).getProducts().get(q));
						}
						for(int q=0;q<sbgnReactions.get(j).getModulators().size();q++){
							//System.out.println("DOT m: "+sbgnReactions.get(j).getModulators().get(q));
						}	*/					
						if(foundEqualReaction(sbgnReactions.get(j),dt,zeros)){
						

							 if(sbgnReactions.get(j).getSbgnType().contains(dt.getType()) || (sbgnReactions.get(j).getSbgnType().contains("State") && dt.getType().contains("State"))){

								 
								 String pos=sbgnReactions.get(j).getMiddlePosition();
								 Utils.debugMsg("\n\nFOUND reaction: "+dt.getName()+"\n\n");
								 dt.setPosition(new Position(pos.substring(0,pos.indexOf(",")),pos.substring(pos.indexOf(",")+1,pos.length()),"0","0"));
								 reactions[i].setUserObject(dt);
								 if(reactions[i] instanceof StateTransition){
									 StateTransition st=(StateTransition)reactions[i];
									 st.setPosition(dt.getPosition().getX(),dt.getPosition().getY());
									 pos=sbgnReactions.get(j).getLeftPosition();
									 st.getLEFTIntermediateVertex().setXYApply(Double.parseDouble(pos.substring(0,pos.indexOf(","))),Double.parseDouble(pos.substring(pos.indexOf(",")+1,pos.length())));
									 pos=sbgnReactions.get(j).getRightPosition();
									 st.getRIGHTIntermediateVertex().setXYApply(Double.parseDouble(pos.substring(0,pos.indexOf(","))),Double.parseDouble(pos.substring(pos.indexOf(",")+1,pos.length())));
									 
								 }else if(reactions[i] instanceof Association){
									 Association st=(Association)reactions[i];
									 st.setPosition(dt.getPosition().getX(),dt.getPosition().getY());
									 pos=sbgnReactions.get(j).getLeftPosition();
									 st.getLEFTIntermediateVertex().setXYApply(Double.parseDouble(pos.substring(0,pos.indexOf(","))),Double.parseDouble(pos.substring(pos.indexOf(",")+1,pos.length())));
									
								 }else if(reactions[i] instanceof Dissociation){
									 Dissociation st=(Dissociation)reactions[i];
									 st.setPosition(dt.getPosition().getX(),dt.getPosition().getY());
									 pos=sbgnReactions.get(j).getRightPosition();
									 st.getRIGHTIntermediateVertex().setXYApply(Double.parseDouble(pos.substring(0,pos.indexOf(","))),Double.parseDouble(pos.substring(pos.indexOf(",")+1,pos.length())));
									
								 }else if(reactions[i] instanceof ReversibleStateTransition){
									 ReversibleStateTransition st=(ReversibleStateTransition)reactions[i];
									 ((MiddleEdge)st.getMiddleEdge1()).getMIDDLEIntermediateVertex().setBounds(dt.getPosition().getX(),dt.getPosition().getY());
									 pos=sbgnReactions.get(j).getLeftPosition();
									 st.getIntermediateVertexLEFT1().setXYApply(Double.parseDouble(pos.substring(0,pos.indexOf(","))),Double.parseDouble(pos.substring(pos.indexOf(",")+1,pos.length())));
									 pos=sbgnReactions.get(j).getRightPosition();
									 st.getIntermediateVertexRIGHT1().setXYApply(Double.parseDouble(pos.substring(0,pos.indexOf(","))),Double.parseDouble(pos.substring(pos.indexOf(",")+1,pos.length())));
									 pos=sbgnReactions.get(j+1).getMiddlePosition();
									 ((MiddleEdge)st.getMiddleEdge2()).getMIDDLEIntermediateVertex().setBounds(Double.parseDouble(pos.substring(0,pos.indexOf(","))),Double.parseDouble(pos.substring(pos.indexOf(",")+1,pos.length())));
									 pos=sbgnReactions.get(j+1).getRightPosition();
									 st.getIntermediateVertexLEFT2().setXYApply(Double.parseDouble(pos.substring(0,pos.indexOf(","))),Double.parseDouble(pos.substring(pos.indexOf(",")+1,pos.length())));
									 pos=sbgnReactions.get(j+1).getLeftPosition();
									 st.getIntermediateVertexRIGHT2().setXYApply(Double.parseDouble(pos.substring(0,pos.indexOf(","))),Double.parseDouble(pos.substring(pos.indexOf(",")+1,pos.length())));
								 }
								 //identify the source/sink molecule of the reaction---->POSITION IT!!!!!!!!!!!!!!
								 
								 for(int k=0;k<sbgnReactions.get(j).getReactants().size();k++){
									 if(zeros.contains(sbgnReactions.get(j).getReactants().get(k))){
										 if(dt.getReactantsAsStrings().contains("Source/Sink")){
											 DefaultGraphCell cl=GraphUtilities.getCellById(graph,dt.getReactants().get(0).getId());
											 if(cl!=null){
												 String poss=positions.get(sbgnReactions.get(j).getReactants().get(k));
												 if(GraphUtilities.getBiochamEntityDataFromCell(cl)!=null){												 
													 GraphUtilities.getBiochamEntityDataFromCell(cl).setPosition(new Position(poss.substring(0,poss.indexOf(",")),poss.substring(poss.indexOf(",")+1,poss.length()),"0","0"));												 
												 }else{
													 BiochamEntityData md=new BiochamEntityData(graph);
													 md.setName("Source/Sink");												 
													 md.setPosition(new Position(poss.substring(0,poss.indexOf(",")),poss.substring(poss.indexOf(",")+1,poss.length()),"0","0"));
													 cl.setUserObject(md);
												 }
												 ((ESourceSink)cl).setPosition(graph,Double.parseDouble(poss.substring(0,poss.indexOf(","))),Double.parseDouble(poss.substring(poss.indexOf(",")+1,poss.length())));
											 }
										 }else if(dt.getProductsAsStrings().contains("Source/Sink")){
											 DefaultGraphCell cl=GraphUtilities.getCellById(graph,dt.getProducts().get(0).getId());
											 if(cl!=null){
												 String poss=positions.get(sbgnReactions.get(j).getProducts().get(k));
												 if(GraphUtilities.getBiochamEntityDataFromCell(cl)!=null){												 
													 GraphUtilities.getBiochamEntityDataFromCell(cl).setPosition(new Position(poss.substring(0,poss.indexOf(",")),poss.substring(poss.indexOf(",")+1,poss.length()),"0","0"));												 
												 }else{
													 BiochamEntityData md=new BiochamEntityData(graph);
													 md.setName("Source/Sink");												 
													 md.setPosition(new Position(poss.substring(0,poss.indexOf(",")),poss.substring(poss.indexOf(",")+1,poss.length()),"0","0"));
													 cl.setUserObject(md);
												 }
												 ((ESourceSink)cl).setPosition(graph,Double.parseDouble(poss.substring(0,poss.indexOf(","))),Double.parseDouble(poss.substring(poss.indexOf(",")+1,poss.length())));
											 }
										 }
									 }
								 }
								 for(int k=0;k<sbgnReactions.get(j).getProducts().size();k++){
									 if(zeros.contains(sbgnReactions.get(j).getProducts().get(k))){
										 if(dt.getProductsAsStrings().contains("Source/Sink")){
											 BiochamEnitity cc=dt.getProducts().get(0);
											 String sid=cc.getId().toString();						 
											 
											 DefaultGraphCell cl=GraphUtilities.getCellById(graph,sid);
											 if(cl!=null){
												 String poss=positions.get(sbgnReactions.get(j).getProducts().get(k));
												 if(GraphUtilities.getBiochamEntityDataFromCell(cl)!=null){												 
													 GraphUtilities.getBiochamEntityDataFromCell(cl).setPosition(new Position(poss.substring(0,poss.indexOf(",")),poss.substring(poss.indexOf(",")+1,poss.length()),"0","0"));												 
												 }else{
													 BiochamEntityData md=new BiochamEntityData(graph);
													 md.setName("Source/Sink");												 
													 md.setPosition(new Position(poss.substring(0,poss.indexOf(",")),poss.substring(poss.indexOf(",")+1,poss.length()),"0","0"));
													 cl.setUserObject(md);
												 }
												 ((ESourceSink)cl).setPosition(graph,Double.parseDouble(poss.substring(0,poss.indexOf(","))),Double.parseDouble(poss.substring(poss.indexOf(",")+1,poss.length())));
											 }
										 }else if(dt.getReactantsAsStrings().contains("Source/Sink")){
											 DefaultGraphCell cl=GraphUtilities.getCellById(graph,dt.getReactants().get(0).getId());
											 if(cl!=null){
												 String poss=positions.get(sbgnReactions.get(j).getReactants().get(k));
												 if(GraphUtilities.getBiochamEntityDataFromCell(cl)!=null){												 
													 GraphUtilities.getBiochamEntityDataFromCell(cl).setPosition(new Position(poss.substring(0,poss.indexOf(",")),poss.substring(poss.indexOf(",")+1,poss.length()),"0","0"));												 
												 }else{
													 BiochamEntityData md=new BiochamEntityData(graph);
													 md.setName("Source/Sink");												 
													 md.setPosition(new Position(poss.substring(0,poss.indexOf(",")),poss.substring(poss.indexOf(",")+1,poss.length()),"0","0"));
													 cl.setUserObject(md);
												 }
												 ((ESourceSink)cl).setPosition(graph,Double.parseDouble(poss.substring(0,poss.indexOf(","))),Double.parseDouble(poss.substring(poss.indexOf(",")+1,poss.length())));
											 }
										 }
									 }
								 }
								 break;
							 }
						 }
					 }
				 }
			 }
			 DefaultGraphCell[] mols=GraphUtilities.getAllMoleculeCells(graph);
			 ArrayList<String> molsGraph=new ArrayList<String>(mols.length);
			// System.out.println("mols size=: "+mols.length);
			 for(int p=0;p<mols.length;p++){
				 molsGraph.add(GraphUtilities.getBiochamEntityDataFromCell(mols[p]).getName());
			 }
			 
			 BiochamEntityData md;			
			 
			 for(int i=0;i<sbgnMols.size();i++){//for all the molecules in the layout............
				 
				 String nm=sbgnMols.get(i).getName();
				 if(nm.startsWith("\"")){
					 nm=nm.substring(1);
				 }
				 if(nm.endsWith("\"")){
					 nm=nm.substring(0,nm.length()-1);
				 }
				
				 
				 if(molsGraph.contains(nm)){
					 
					 
					 	
					 DefaultGraphCell cell=GraphUtilities.getCellByName(graph, nm);
					 if(cell!=null){
						 md=GraphUtilities.getBiochamEntityDataFromCell(cell);	
						
						 if(md!=null){
							 
							 String pos=positions.get(sbgnMols.get(i).getName());
							// System.out.println("dot MOL: "+nm+",pos="+pos);
							 
							 if(pos!=null){
								 md.setPosition(new Position(pos.substring(0,pos.indexOf(",")),pos.substring(pos.indexOf(",")+1,pos.length()),"0","0"));
								 if(cell instanceof EMacromoleculeCell){
									 ((EMacromoleculeCell)cell).setPosition(graph,md.getPosition().getX(),md.getPosition().getY());
								 }else if(cell instanceof EComplexCell){
									 ((EComplexCell)cell).setPosition(graph,md.getPosition().getX(),md.getPosition().getY());
								 }else if(cell instanceof ENucleicAcidFeatureCell){
									 ((ENucleicAcidFeatureCell)cell).setPosition(graph,md.getPosition().getX(),md.getPosition().getY());
								 }else if(cell instanceof ESourceSink){
									 ((ESourceSink)cell).setPosition(graph,md.getPosition().getX(),md.getPosition().getY());
								 }
							 }	
							
							 
						 }		
					 }
					 			 						
				 }
			 }

			 /*for(int i=0;i<mols.length;i++){//for all the molecules of the graph				 
			
				 
				 md=GraphUtilities.getBiochamEntityDataFromCell(mols[i]);
				 System.out.println("graph MOL: "+mols[i]);
				 
				 
				 if(md!=null){
				
					 
					 for(int j=0;j<sbgnMols.size();j++){//for all the molecules in the layout.............
					
						 
						 
						 System.out.println("dot MOL: "+sbgnMols.get(j).getName());
						 if(sbgnMols.get(j).getName().contains(md.getName())){
							 String pos=positions.get(md.getName());
							 if(pos!=null){
								 md.setPosition(new Position(pos.substring(1,pos.indexOf(",")),pos.substring(pos.indexOf(",")+1,pos.length()-1),"0","0"));
								 if(mols[i] instanceof EMacromoleculeCell){
									 ((EMacromoleculeCell)mols[i]).setPosition(graph,md.getPosition().getX(),md.getPosition().getY());
								 }else if(mols[i] instanceof EComplexCell){
									 ((EComplexCell)mols[i]).setPosition(graph,md.getPosition().getX(),md.getPosition().getY());
								 }else if(mols[i] instanceof ENucleicAcidFeatureCell){
									 ((ENucleicAcidFeatureCell)mols[i]).setPosition(graph,md.getPosition().getX(),md.getPosition().getY());
								 }else if(mols[i] instanceof ESourceSink){
									 ((ESourceSink)mols[i]).setPosition(graph,md.getPosition().getX(),md.getPosition().getY());
								 }
							 }							
						 }	 
					 }
				 }	 
			 }*/
						 
					/*	 int counter=0;
						 int size=sbgnReactions.get(j).getElements().size();
						 for(int k=0;k<sbgnReactions.get(j).getElements().size();k++){
							 String mol=sbgnReactions.get(j).getElements().get(k);
							 System.out.println("mol="+mol);
							 if(dt.getReactantsAsStrings().contains(sbgnReactions.get(j).getElements().get(k)) || dt.getProductsAsStrings().contains(sbgnReactions.get(j).getElements().get(k)) || dt.getAllModulatorsAsStrings().contains(sbgnReactions.get(j).getElements().get(k))){
								 counter++;		 
							 }
						 }
						 if(counter>=sbgnReactions.get(j).getElements().size()-2){
							 System.out.println("\n\nFOUND reaction: "+dt.getName()+"\n\n");
							 String pos=sbgnReactions.get(j).getPosition();
							 dt.setPosition(new Position(pos.substring(1,pos.indexOf(",")),pos.substring(pos.indexOf(",")+1,pos.length()-1),"0","0"));
							 reactions[i].setUserObject(dt);
							 if(reactions[i] instanceof StateTransition){
								 StateTransition st=(StateTransition)reactions[i];
								 st.setPosition(dt.getPosition().getX(),dt.getPosition().getY());
								 
							 }else if(reactions[i] instanceof Association){
								 Association st=(Association)reactions[i];
								 st.setPosition(dt.getPosition().getX(),dt.getPosition().getY());
								 
							 }else if(reactions[i] instanceof Dissociation){
								 Dissociation st=(Dissociation)reactions[i];
								 st.setPosition(dt.getPosition().getX(),dt.getPosition().getY());
								 
							 }
							 //identify the source/sink molecule of the reaction---->POSITION IT!!!!!!!!!!!!!!
							 
							 for(int k=0;k<sbgnReactions.get(j).getElements().size();k++){
								 if(zeros.contains(sbgnReactions.get(j).getElements().get(k))){
									 if(dt.getReactantsAsStrings().contains("Source/Sink")){
										 DefaultGraphCell cl=GraphUtilities.getCellById(graph,dt.getReactants().get(0).getId());
										 if(cl!=null){
											 String poss=positions.get(sbgnReactions.get(j).getElements().get(k));
											 if(GraphUtilities.getBiochamEntityDataFromCell(cl)!=null){												 
												 GraphUtilities.getBiochamEntityDataFromCell(cl).setPosition(new Position(poss.substring(1,poss.indexOf(",")),poss.substring(poss.indexOf(",")+1,poss.length()-1),"0","0"));												 
											 }else{
												 BiochamEntityData md=new BiochamEntityData(graph);
												 md.setName("Source/Sink");												 
												 md.setPosition(new Position(poss.substring(1,poss.indexOf(",")),poss.substring(poss.indexOf(",")+1,poss.length()-1),"0","0"));
												 cl.setUserObject(md);
											 }
											 ((ESourceSink)cl).setPosition(graph,Double.parseDouble(poss.substring(1,poss.indexOf(","))),Double.parseDouble(poss.substring(poss.indexOf(",")+1,poss.length()-1)));
										 }
									 }else if(dt.getProductsAsStrings().contains("Source/Sink")){
										 DefaultGraphCell cl=GraphUtilities.getCellById(graph,dt.getProducts().get(0).getId());
										 if(cl!=null){
											 String poss=positions.get(sbgnReactions.get(j).getElements().get(k));
											 if(GraphUtilities.getBiochamEntityDataFromCell(cl)!=null){												 
												 GraphUtilities.getBiochamEntityDataFromCell(cl).setPosition(new Position(poss.substring(1,poss.indexOf(",")),poss.substring(poss.indexOf(",")+1,poss.length()-1),"0","0"));												 
											 }else{
												 BiochamEntityData md=new BiochamEntityData(graph);
												 md.setName("Source/Sink");												 
												 md.setPosition(new Position(poss.substring(1,poss.indexOf(",")),poss.substring(poss.indexOf(",")+1,poss.length()-1),"0","0"));
												 cl.setUserObject(md);
											 }
											 ((ESourceSink)cl).setPosition(graph,Double.parseDouble(poss.substring(1,poss.indexOf(","))),Double.parseDouble(poss.substring(poss.indexOf(",")+1,poss.length()-1)));
										 }
									 }
								 }
							 }
							 
							 
						 }*/
					 
			 graph.refresh();
			
		} catch (FileNotFoundException e) {
			JOptionPane.showMessageDialog(BiochamMainFrame.frame,"Not correct input file");
			e.printStackTrace();
		} catch (IOException e) {
			JOptionPane.showMessageDialog(BiochamMainFrame.frame,"Not correct input file");
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		finally{
			if(layoutFromDOT){
				f.delete();
			}
		}

		
	}

	private static boolean foundEqualReaction(DotReaction reaction, BiochamEdgeData data, ArrayList<String> zeros) {		
		//sbgnReactions.get(j).getReactants().size()==dt.getReactants().size() && 
		//sbgnReactions.get(j).getProducts().size()==dt.getProducts().size() &&
		//sbgnReactions.get(j).getModulators().size()==dt.getAllModulatorsAsStrings().size())
		boolean dblReversible=data.getReversibilityType()==ReversibilityType.DOUBLE;
		
		int s1=reaction.getReactants().size();
		int s2=reaction.getProducts().size();
		int s3=reaction.getModulators().size();
		int s11=data.getReactants().size();
		String type=data.getType();
		int s22=data.getProducts().size();
		int s33=data.getAllModulatorsAsStrings().size();		
		if(s1==s11 && s2==s22 && s3==s33){
			for(int i=0;i<s11;i++){
				String s=reaction.getReactants().get(i);
				
				boolean c=zeros.contains(s);
				if(!c){
					if(s.startsWith("\"")){
						s=s.substring(1);
					}
					if(s.endsWith("\"")){
						s=s.substring(0,s.length()-1);
					}
				}
				if(dblReversible){
					if(!(data.getReactantsAsStrings().contains(s) || data.getProductsAsStrings().contains(s) || c)){
						return false;
					}
				}else{
					if(!(data.getReactantsAsStrings().contains(s) || c)){
						return false;
					}	
				}
				
			}
			for(int i=0;i<s22;i++){
				String s=reaction.getProducts().get(i);				
				boolean c=zeros.contains(s);
				if(!c){
					if(s.startsWith("\"")){
						s=s.substring(1);
					}
					if(s.endsWith("\"")){
						s=s.substring(0,s.length()-1);
					}
				}
				if(dblReversible){
					if(!(data.getProductsAsStrings().contains(s) || data.getReactantsAsStrings().contains(s) || c)){
						return false;
					}
				}else{
					if(!(data.getProductsAsStrings().contains(s) || c)){
						return false;
					}
				}
				
			}
			for(int i=0;i<s33;i++){
				String s=reaction.getModulators().get(i);
				if(s.startsWith("\"")){
					s=s.substring(1);
				}
				if(s.endsWith("\"")){
					s=s.substring(0,s.length()-1);
				}
				if(!data.getAllModulatorsAsStrings().contains(s)){
					return false;
				}
			}
		}else{
			return false;
		}		
		
		Utils.debugMsg(data.getName()+" is equal with "+reaction.getSbgnType());
		return true;
	}
	
}






















