package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultGraphCell;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;
import java.util.UUID;

public class SbgnGraphToDot {

	static String macromolDefaultColor="\"#66CDAA\"";
	static String geneDefaultColor="\"#4169E1\"";
	static String complexMolDefaultColor="\"#E0B0FF\"";
	static String modulatorMolDefaultColor="\"#8B0000\"";
	
	static String stateTransition=" [label=\"\",shape=rect, width=.2 height=.2";
	static String association=" [label=\"\",shape=circle, style=\"filled\", color=\"black\",width=.2 height=.2";
	static String dissociation=" [label=\"O\",shape=circle, fixedsize=true, width=.2, height=.2";
	static String invisible=" [label=\"\", style=invis, width=0.000, height=0.000";
	
	static String fin="];\n";
	static String zero="[label=\"/\",shape=circle";
	
	// StateTransition [label="",shape=rect, width=.2 height=.2];
	// Associaton [label="",shape=circle, style="filled", color="black",width=.2 height=.2];
	// Dissociation [label="O",shape=circle,width=.2, height=.2,fixedsize=true];
	// SourceSink [label="/",shape=circle];
	// Macromolecule [label="moleculeName",shape=box,fillcolor="macromolColor=palegreen",style="filled,rounded"];
	// Gene [label="geneName",shape=rectangle,fillcolor="geneColor=blue",style="filled"];
	// Complex [label="complexName",shape=octagon,style="filled",fillcolor="complexColor"];
	
	
	public static void save(final BiochamGraph graph, final boolean forDotLayout, final File tmpFile,final String type){
		
		SwingWorker sw=new SwingWorker(){

			@Override
			public Object construct() {
				
				File file = null;
				
				if(!forDotLayout || tmpFile==null){
					String rep=Utils.showSaveDialog("",BiochamMainFrame.frame,"dot");			
					
					if (rep!=null) {
						if(!rep.endsWith("dot")){
							rep+=".dot";
						}
						file=new File(rep);
					}else{
						file=new File(Utils.getNameWithoutExtension(graph.getBiochamModel().getModelName()+".dot"));
					}
				}else{
					file=tmpFile;
				}
				String nm=file.getAbsolutePath();
				
				StringBuffer buffer=new StringBuffer();	
				if(type!=null){
					if(type.contains("TB")){
						buffer.append("digraph g{\n  overlap=false;\n ranksep=\".8 equally\";\n  nodesep=\"2 equally\";\n  rankdir=TB;\n  concentrate=false;\n");
					}else{
						buffer.append("digraph g{\n  overlap=false;\n ranksep=\".8 equally\";\n  nodesep=\"2 equally\";\n  rankdir=LR;\n  concentrate=false;\n");	
					}
				}else{
					buffer.append("digraph g{\n  overlap=false;\n ranksep=\".8 equally\";\n  nodesep=\"2 equally\";\n  rankdir=LR;\n  concentrate=false;\n");
				}
				
				
				DefaultGraphCell[] rs=GraphUtilities.getAllReactions(graph);								
				
				
				Utils.debugMsg(type);
				ArrayList<String> writtenMolecules=new ArrayList<String>();
				//ArrayList<String> sourceSinks=new ArrayList<String>();
				HashMap<DotSourceSink,String> tm=new HashMap<DotSourceSink,String>();
				DotSourceSink so;
				
				for(int i=0;i<rs.length;i++){			
					
					/**Write the molecules.....*/

					BiochamEdgeData dt=GraphUtilities.getBiochamEdgeDataFromCell(rs[i]);
				
					
					for(int j=0;j<dt.getReactants().size();j++){
												
						if(dt.getReactants().get(j).getName().equals("Source/Sink")){
							buffer.append("  \"");
							String ss=UUID.randomUUID().toString().replaceAll("-","");
							buffer.append(ss);
							buffer.append("\"");
							buffer.append(" [label=\"/\",shape=circle");
							if(dt.getReactants().get(j).getId()!=null){
								if(!forDotLayout){
									BiochamEntityData edt=GraphUtilities.getBiochamEntityDataFromCell(GraphUtilities.getCellById(graph,dt.getReactants().get(j).getId()));
									if(edt!=null){
										if(edt.getPosition()!=null){
											buffer.append(", pos=\"");
											buffer.append(edt.getPosition().getX());
											buffer.append(",");
											buffer.append(edt.getPosition().getY());
											buffer.append("\"");
										}
									}
								}								
							}
							buffer.append("];\n");
							so=new DotSourceSink(ss,dt.getName(),"r");
							tm.put(so,dt.getName());
							//sourceSinks.add(ss);
							//if(dt.isReversible()){
								//sourceSinks.add(ss);
							//}
						}else{
							DefaultGraphCell c=GraphUtilities.getCellByName(graph,dt.getReactants().get(j).getName());
							if(c!=null){
								BiochamEntityData cd=GraphUtilities.getBiochamEntityDataFromCell(c);
								if(cd!=null || (c instanceof ESourceSink)){
																	
									if(c instanceof EMacromoleculeCell){		
										//[label="moleculeName",shape=box,fillcolor="macromolColor=palegreen",style="filled,rounded"];
										if(!writtenMolecules.contains(cd.getName())){
											buffer.append("  \"");
											buffer.append(cd.getName());
											buffer.append("\"");
											writtenMolecules.add(cd.getName());
											buffer.append(" [label=\"");
											buffer.append(cd.getName());
											buffer.append("\", shape=box,style=\"filled,rounded\", fillcolor=");
											if(cd.getColor()!=null){
												buffer.append("\"");
												//"\"#66CDAA\""
												buffer.append("#"+Integer.toHexString(cd.getColor().getRGB()));
												buffer.append("\"");
											}else{	
												if(cd.isModulator()){
													buffer.append(modulatorMolDefaultColor);
												}else{
													buffer.append(macromolDefaultColor);
												}
											}
											if(!forDotLayout){
												if(cd.getSize()!=null){
													buffer.append(", width=");
													buffer.append(cd.getSize().getWidth());
													buffer.append(", height=");
													buffer.append(cd.getSize().getHeight());												
												}	
												if(cd.getPosition()!=null){
													buffer.append(", pos=\"");
													buffer.append(cd.getPosition().getX());
													buffer.append(",");
													buffer.append(cd.getPosition().getY());
													buffer.append("\"");
												}	
											}											
																	
											buffer.append("];\n");
										}
																	
										
									}else if(c instanceof ENucleicAcidFeatureCell){
										//[label="geneName",shape=rectangle,fillcolor="geneColor=blue",style="filled"];
										if(!writtenMolecules.contains(cd.getName())){
											buffer.append("  \"");
											buffer.append(cd.getName());
											buffer.append("\"");
											writtenMolecules.add(cd.getName());
											buffer.append(" [label=\"");
											buffer.append(cd.getName());										
											buffer.append("\", shape=rectangle,style=\"filled\", fillcolor=");
											if(cd.getColor()!=null){
												buffer.append("\"");
												buffer.append("#"+Integer.toHexString(cd.getColor().getRGB()));
												buffer.append("\"");
											}else{
												if(cd.isModulator()){
													buffer.append(modulatorMolDefaultColor);
												}else{
													buffer.append(geneDefaultColor);
												}												
											}	
											if(!forDotLayout){
												if(cd.getSize()!=null){
													buffer.append(", width=");
													buffer.append(cd.getSize().getWidth());
													buffer.append(", height=");
													buffer.append(cd.getSize().getHeight());												
												}
												if(cd.getPosition()!=null){
													buffer.append(", pos=\"");
													buffer.append(cd.getPosition().getX());
													buffer.append(",");
													buffer.append(cd.getPosition().getY());
													buffer.append("\"");
												}	
											}											
												
											buffer.append("];\n");
										}
										
									}else  if(c instanceof EComplexCell){
										//[label="complexName",shape=octagon,style="filled",fillcolor="complexColor"];
										if(!writtenMolecules.contains(cd.getName())){
											buffer.append("  \"");
											buffer.append(cd.getName());
											buffer.append("\"");
											writtenMolecules.add(cd.getName());
											buffer.append(" [label=\"");
											buffer.append(cd.getName());
											buffer.append("\", shape=octagon,style=\"filled\", fillcolor=");
											if(cd.getColor()!=null){
												buffer.append("\"");
												buffer.append("#"+Integer.toHexString(cd.getColor().getRGB()));
												buffer.append("\"");
											}else{
												if(cd.isModulator()){
													buffer.append(modulatorMolDefaultColor);
												}else{
													buffer.append(complexMolDefaultColor);
												}												
											}
											if(!forDotLayout){
												if(cd.getSize()!=null){
													buffer.append(", width=");
													buffer.append(cd.getSize().getWidth());
													buffer.append(", height=");
													buffer.append(cd.getSize().getHeight());												
												}	
												if(cd.getPosition()!=null){
													buffer.append(", pos=\"");
													buffer.append(cd.getPosition().getX());
													buffer.append(",");
													buffer.append(cd.getPosition().getY());
													buffer.append("\"");
												}	
											}												
											buffer.append("];\n");
										}
										
									}
									
								}
							}
						}
						
					}
					for(int j=0;j<dt.getAllModulatorsAsStrings().size();j++){
						
							DefaultGraphCell c=GraphUtilities.getCellByName(graph,dt.getAllModulatorsAsStrings().get(j));
							if(c!=null){
								BiochamEntityData cd=GraphUtilities.getBiochamEntityDataFromCell(c);
								if(cd!=null || (c instanceof ESourceSink)){
																	
									if(c instanceof EMacromoleculeCell){		
										//[label="moleculeName",shape=box,fillcolor="macromolColor=palegreen",style="filled,rounded"];
										if(!writtenMolecules.contains(cd.getName())){
											buffer.append("  \"");
											buffer.append(cd.getName());
											buffer.append("\"");
											writtenMolecules.add(cd.getName());
											buffer.append(" [label=\"");
											buffer.append(cd.getName());
											buffer.append("\", shape=box,style=\"filled,rounded\", fillcolor=");
											if(cd.getColor()!=null){
												buffer.append("\"");
												buffer.append("#"+Integer.toHexString(cd.getColor().getRGB()));
												buffer.append("\"");
											}else{											
												buffer.append(modulatorMolDefaultColor);
											}
											if(!forDotLayout){
												if(cd.getSize()!=null){
													buffer.append(", width=");
													buffer.append(cd.getSize().getWidth());
													buffer.append(", height=");
													buffer.append(cd.getSize().getHeight());												
												}	
												if(cd.getPosition()!=null){
													buffer.append(", pos=\"");
													buffer.append(cd.getPosition().getX());
													buffer.append(",");
													buffer.append(cd.getPosition().getY());
													buffer.append("\"");
												}		
											}											
											buffer.append("];\n");
										}
																	
										
									}else if(c instanceof ENucleicAcidFeatureCell){
										//[label="geneName",shape=rectangle,fillcolor="geneColor=blue",style="filled"];
										if(!writtenMolecules.contains(cd.getName())){
											buffer.append("  \"");
											buffer.append(cd.getName());
											buffer.append("\"");
											writtenMolecules.add(cd.getName());
											buffer.append(" [label=\"");
											buffer.append(cd.getName());										
											buffer.append("\", shape=rectangle,style=\"filled\", fillcolor=");
											if(cd.getColor()!=null){
												buffer.append("\"");
												buffer.append("#"+Integer.toHexString(cd.getColor().getRGB()));
												buffer.append("\"");
											}else{
												buffer.append(modulatorMolDefaultColor);
											}
											if(!forDotLayout){
												if(cd.getSize()!=null){
													buffer.append(", width=");
													buffer.append(cd.getSize().getWidth());
													buffer.append(", height=");
													buffer.append(cd.getSize().getHeight());												
												}	
												if(cd.getPosition()!=null){
													buffer.append(", pos=\"");
													buffer.append(cd.getPosition().getX());
													buffer.append(",");
													buffer.append(cd.getPosition().getY());
													buffer.append("\"");
												}		
											}											
											buffer.append("];\n");
										}
										
									}else  if(c instanceof EComplexCell){
										//[label="complexName",shape=octagon,style="filled",fillcolor="complexColor"];
										if(!writtenMolecules.contains(cd.getName())){
											buffer.append("  \"");
											buffer.append(cd.getName());
											buffer.append("\"");
											writtenMolecules.add(cd.getName());
											buffer.append(" [label=\"");
											buffer.append(cd.getName());
											buffer.append("\", shape=octagon,style=\"filled\", fillcolor=");
											if(cd.getColor()!=null){
												buffer.append("\"");
												buffer.append("#"+Integer.toHexString(cd.getColor().getRGB()));
												buffer.append("\"");
											}else{
												buffer.append(modulatorMolDefaultColor);
											}	
											if(!forDotLayout){
												if(cd.getSize()!=null){
													buffer.append(", width=");
													buffer.append(cd.getSize().getWidth());
													buffer.append(", height=");
													buffer.append(cd.getSize().getHeight());												
												}	
												if(cd.getPosition()!=null){
													buffer.append(", pos=\"");
													buffer.append(cd.getPosition().getX());
													buffer.append(",");
													buffer.append(cd.getPosition().getY());
													buffer.append("\"");
												}		
											}											
											buffer.append("];\n");
										}
										
									}
									
								}
							}
					}
					for(int j=0;j<dt.getProducts().size();j++){
						if(dt.getProducts().get(j).getName().equals("Source/Sink")){
							buffer.append("  \"");
							String ss=UUID.randomUUID().toString().replaceAll("-","");
							buffer.append(ss);
							buffer.append("\"");
							buffer.append(" [label=\"/\",shape=circle");
							if(dt.getProducts().get(j).getId()!=null){
								if(!forDotLayout){
									BiochamEntityData edt=GraphUtilities.getBiochamEntityDataFromCell(GraphUtilities.getCellById(graph,dt.getProducts().get(j).getId()));
									if(edt!=null){
										if(edt.getPosition()!=null){
											buffer.append(", pos=\"");
											buffer.append(edt.getPosition().getX());
											buffer.append(",");
											buffer.append(edt.getPosition().getY());
											buffer.append("\"");
										}
									}
								}							
							}
							buffer.append("];\n");
							so=new DotSourceSink(ss,dt.getName(),"p");
							tm.put(so,dt.getName());
							
						}else{
							DefaultGraphCell c=GraphUtilities.getCellByName(graph,dt.getProducts().get(j).getName());
							if(c!=null){
								BiochamEntityData cd=GraphUtilities.getBiochamEntityDataFromCell(c);
								if(cd!=null || (c instanceof ESourceSink)){
																	
									if(c instanceof EMacromoleculeCell){		
										//[label="moleculeName",shape=box,fillcolor="macromolColor=palegreen",style="filled,rounded"];
										if(!writtenMolecules.contains(cd.getName())){
											buffer.append("  \"");
											buffer.append(cd.getName());
											buffer.append("\"");
											writtenMolecules.add(cd.getName());
											buffer.append(" [label=\"");
											buffer.append(cd.getName());
											buffer.append("\", shape=box,style=\"filled,rounded\", fillcolor=");
											if(cd.getColor()!=null){
												buffer.append("\"");
												buffer.append("#"+Integer.toHexString(cd.getColor().getRGB()));
												buffer.append("\"");
											}else{											
												if(cd.isModulator()){
													buffer.append(modulatorMolDefaultColor);
												}else{
													buffer.append(macromolDefaultColor);
												}
											}
											if(!forDotLayout){
												if(cd.getSize()!=null){
													buffer.append(", width=");
													buffer.append(cd.getSize().getWidth());
													buffer.append(", height=");
													buffer.append(cd.getSize().getHeight());												
												}	
												if(cd.getPosition()!=null){
													buffer.append(", pos=\"");
													buffer.append(cd.getPosition().getX());
													buffer.append(",");
													buffer.append(cd.getPosition().getY());
													buffer.append("\"");
												}		
											}											
											buffer.append("];\n");
										}
																	
										
									}else if(c instanceof ENucleicAcidFeatureCell){
										//[label="geneName",shape=rectangle,fillcolor="geneColor=blue",style="filled"];
										if(!writtenMolecules.contains(cd.getName())){
											buffer.append("  \"");
											buffer.append(cd.getName());
											buffer.append("\"");
											writtenMolecules.add(cd.getName());
											buffer.append(" [label=\"");
											buffer.append(cd.getName());										
											buffer.append("\", shape=rectangle,style=\"filled\", fillcolor=");
											if(cd.getColor()!=null){
												buffer.append("\"");
												buffer.append("#"+Integer.toHexString(cd.getColor().getRGB()));
												buffer.append("\"");
											}else{
												if(cd.isModulator()){
													buffer.append(modulatorMolDefaultColor);
												}else{
													buffer.append(geneDefaultColor);
												}	
											}	
											if(!forDotLayout){
												if(cd.getSize()!=null){
													buffer.append(", width=");
													buffer.append(cd.getSize().getWidth());
													buffer.append(", height=");
													buffer.append(cd.getSize().getHeight());												
												}	
												if(cd.getPosition()!=null){
													buffer.append(", pos=\"");
													buffer.append(cd.getPosition().getX());
													buffer.append(",");
													buffer.append(cd.getPosition().getY());
													buffer.append("\"");
												}		
											}											
											buffer.append("];\n");
										}
										
									}else  if(c instanceof EComplexCell){
										//[label="complexName",shape=octagon,style="filled",fillcolor="complexColor"];
										if(!writtenMolecules.contains(cd.getName())){
											buffer.append("  \"");
											buffer.append(cd.getName());
											buffer.append("\"");
											writtenMolecules.add(cd.getName());
											buffer.append(" [label=\"");
											buffer.append(cd.getName());
											buffer.append("\", shape=octagon,style=\"filled\", fillcolor=");
											if(cd.getColor()!=null){
												buffer.append("\"");
												buffer.append("#"+Integer.toHexString(cd.getColor().getRGB()));
												buffer.append("\"");
											}else{
												if(cd.isModulator()){
													buffer.append(modulatorMolDefaultColor);
												}else{
													buffer.append(complexMolDefaultColor);
												}	
											}
											if(!forDotLayout){
												if(cd.getSize()!=null){
													buffer.append(", width=");
													buffer.append(cd.getSize().getWidth());
													buffer.append(", height=");
													buffer.append(cd.getSize().getHeight());												
												}	
												if(cd.getPosition()!=null){
													buffer.append(", pos=\"");
													buffer.append(cd.getPosition().getX());
													buffer.append(",");
													buffer.append(cd.getPosition().getY());
													buffer.append("\"");
												}		
											}											
											buffer.append("];\n");
										}
										
									}
									
								}
							}
						}
					}
					
					/**Write the reactions.....*/
					
					buffer.append("  \"");
					String name=UUID.randomUUID().toString().replaceAll("-","");
					buffer.append(name);
					buffer.append("\"");
					
					Utils.debugMsg("SOURCESINKS.size="+tm.size());
									
					if(rs[i] instanceof StateTransition){						
						/**
						 * a -> im1 [arrowhead=none];
						 * im1 [label="",shape=rect, width=.2 height=.2];
						 * 
						 */		
						buffer.append(invisible);
						StateTransition st=(StateTransition)rs[i];
						if(!forDotLayout){
							buffer.append(", pos=\"");
							buffer.append(st.getLEFTIntermediateVertex().getX());
							buffer.append(",");
							buffer.append(st.getLEFTIntermediateVertex().getY());
							buffer.append("\"");
						}						
						buffer.append("];\n");
						buffer.append("  \"");
						String name2=UUID.randomUUID().toString().replaceAll("-","");
						buffer.append(name2);
						buffer.append("\"");
						buffer.append(stateTransition);
						if(!forDotLayout){
							buffer.append(", pos=\"");
							buffer.append(st.getMIDDLEIntermediateVertex().getX());
							buffer.append(",");
							buffer.append(st.getMIDDLEIntermediateVertex().getY());
							buffer.append("\"");
						}						
						buffer.append("];\n");
						buffer.append("  \"");
						String name3=UUID.randomUUID().toString().replaceAll("-","");
						buffer.append(name3);
						buffer.append("\"");
						buffer.append(invisible);
						if(!forDotLayout){
							buffer.append(", pos=\"");
							buffer.append(st.getRIGHTIntermediateVertex().getX());
							buffer.append(",");
							buffer.append(st.getRIGHTIntermediateVertex().getY());
							buffer.append("\"");
						}							
						buffer.append("];\n");
						for(int j=0;j<dt.getReactants().size();j++){
							if(dt.getReactants().get(j).name.equals("Source/Sink")){
								buffer.append("  \"");
								if(tm.containsValue(dt.getName())){
									Set keySet = tm.keySet();
									ArrayList<DotSourceSink> keyList = new ArrayList<DotSourceSink>(keySet);
									for(int k=0;k<keyList.size();k++){
									   if(tm.get(keyList.get(k)).equals(dt.getName())){
										   so=keyList.get(k);
										   if(so.getRole().equals("r")){
											   buffer.append(so.getId());
											   //tm.remove(keyList.get(k));
											   break;
										   }
									   }
									}
								}															
								buffer.append("\"");
							}else{
								buffer.append("  ");
								buffer.append("\"");
								buffer.append(dt.getReactants().get(j).name);
								buffer.append("\"");
							}
							
							buffer.append(" -> ");
							buffer.append("\"");
							buffer.append(name);
							buffer.append("\"");
							if(!dt.isReversible()){
								buffer.append(" [arrowhead=none];\n");
							}else{								
								buffer.append(" [arrowhead=none, arrowtail=normal];\n");
							}
														
						}
						
						buffer.append("  \"");
						buffer.append(name);
						buffer.append("\"");
						buffer.append(" -> ");
						buffer.append("\"");
						buffer.append(name2);
						buffer.append("\"");						
						buffer.append(" [arrowhead=none];\n");
						
						buffer.append("  \"");
						buffer.append(name2);
						buffer.append("\"");
						buffer.append(" -> ");
						buffer.append("\"");
						buffer.append(name3);
						buffer.append("\"");
						buffer.append(" [arrowhead=none];\n");
						
						for(int j=0;j<dt.getProducts().size();j++){
							buffer.append("  \"");
							buffer.append(name3);
							buffer.append("\"");							
							buffer.append(" -> ");
							if(dt.getProducts().get(j).name.equals("Source/Sink")){
								buffer.append("  \"");
								Set keySet = tm.keySet();
								ArrayList<DotSourceSink> keyList = new ArrayList<DotSourceSink>(keySet);
								for(int k=0;k<keyList.size();k++){
								   if(tm.get(keyList.get(k)).equals(dt.getName())){
									   so=keyList.get(k);
									   if(so.getRole().equals("p")){
										   buffer.append(so.getId());
										   //tm.remove(keyList.get(k));
										   break;
									   }
								   }
								}
								buffer.append("\"");
							}else{
								buffer.append("  ");
								buffer.append("\"");
								buffer.append(dt.getProducts().get(j).name);
								buffer.append("\"");
							}						
							buffer.append("\n");								
						}
						if(dt.modulators1!=null){
							int s1=dt.getModulators1().size();
							for(int j=0;j<dt.getModulators1().size();j++){	
								buffer.append("  \"");								
								buffer.append(dt.getModulators1().get(j).name);
								buffer.append("\" -> ");
								buffer.append("  \"");
								buffer.append(name2);
								buffer.append("\"");
								buffer.append(" [arrowhead=ediamond];\n");
							}	
						}
						
						
					}else if(rs[i] instanceof Association){
						/**
						 * k2->inv [arrowhead=none]; 
						 * inv [label="", width=0.000, height=0.000, style=invis];
						 * inv->im2 [arrowhead=none,weight=5000];
  						 * im2->f1 
						 */		
						buffer.append(invisible);
						Association st=(Association)rs[i];
						if(!forDotLayout){
							buffer.append(", pos=\"");
							buffer.append(st.getLEFTIntermediateVertex().getX());
							buffer.append(",");
							buffer.append(st.getLEFTIntermediateVertex().getY());
							buffer.append("\"");
						}						
						buffer.append("];\n");
						buffer.append("  \"");
						String as=UUID.randomUUID().toString().replaceAll("-","");
						buffer.append(as);
						buffer.append("\"");
						buffer.append(association);
						if(!forDotLayout){							
							buffer.append(", pos=\"");
							buffer.append(st.getMIDDLEIntermediateVertex().getX());
							buffer.append(",");
							buffer.append(st.getMIDDLEIntermediateVertex().getY());
							buffer.append("\"");
						}
						buffer.append("];\n");
						for(int j=0;j<dt.getReactants().size();j++){
							if(dt.getReactants().get(j).name.equals("Source/Sink")){
								buffer.append("  \"");
								Set keySet = tm.keySet();
								ArrayList<DotSourceSink> keyList = new ArrayList<DotSourceSink>(keySet);
								for(int k=0;k<keyList.size();k++){
								   if(tm.get(keyList.get(k)).equals(dt.getName())){
									   so=keyList.get(k);
									   if(so.getRole().equals("r")){
										   buffer.append(so.getId());
										   //tm.remove(keyList.get(k));
										   break;
									   }
								   }
								}
								buffer.append("\"");
							}else{
								buffer.append("  ");
								buffer.append("\"");
								buffer.append(dt.getReactants().get(j).name);
								buffer.append("\"");
							}
							buffer.append(" -> ");
							buffer.append("  \"");
							buffer.append(name);
							buffer.append("\"");
							buffer.append(" [arrowhead=none];\n");							
						}
						buffer.append("  \"");
						buffer.append(name);
						buffer.append("\"");						
						buffer.append(" -> ");
						buffer.append("\"");
						buffer.append(as);
						buffer.append("\"");
						buffer.append(" [arrowhead=none];\n");								
						for(int j=0;j<dt.getProducts().size();j++){
							buffer.append("  \"");
							buffer.append(as);	
							buffer.append("\"");
							buffer.append(" -> ");
							if(dt.getProducts().get(j).name.equals("Source/Sink")){
								buffer.append("  \"");
								Set keySet = tm.keySet();
								ArrayList<DotSourceSink> keyList = new ArrayList<DotSourceSink>(keySet);
								for(int k=0;k<keyList.size();k++){
								   if(tm.get(keyList.get(k)).equals(dt.getName())){
									   so=keyList.get(k);
									   if(so.getRole().equals("p")){
										   buffer.append(so.getId());
										   //tm.remove(keyList.get(k));
										   break;
									   }
								   }
								}
								buffer.append("\"");
							}else{
								buffer.append("  ");
								buffer.append("\"");
								buffer.append(dt.getProducts().get(j).name);
								buffer.append("\"");
							}					
							buffer.append("\n");								
						}
						if(dt.modulators1!=null){
							for(int j=0;j<dt.getModulators1().size();j++){			
								buffer.append("  \"");
								buffer.append(dt.getModulators1().get(j).name);
								buffer.append("\" -> ");
								buffer.append("\"");
								buffer.append(as);
								buffer.append("\"");
								buffer.append(" [arrowhead=ediamond];\n");
							}	
						}
						if(dt.modulators2!=null){
							for(int j=0;j<dt.getModulators2().size();j++){	
								buffer.append("  \"");
								buffer.append(dt.getModulators2().get(j).name);
								buffer.append("\" -> ");
								buffer.append("\"");
								buffer.append(as);
								buffer.append("\"");
								buffer.append(" [arrowhead=ediamond];\n");
							}	
						}
					}else if(rs[i] instanceof Dissociation){
						/**
						 * k2->inv [arrowhead=none]; 
						 * inv [label="", width=0.000, height=0.000, style=invis];
						 * inv->im2 [arrowhead=none,weight=5000];
  						 * im2->f1 
						 */		
						buffer.append(invisible);
						Dissociation st=(Dissociation)rs[i];
						if(!forDotLayout){							
							buffer.append(", pos=\"");
							buffer.append(st.getRIGHTIntermediateVertex().getX());
							buffer.append(",");
							buffer.append(st.getRIGHTIntermediateVertex().getY());
							buffer.append("\"");
						}						
						buffer.append("];\n");
						buffer.append("  \"");
						String ds=UUID.randomUUID().toString().replaceAll("-","");
						buffer.append(ds);
						buffer.append("\"");
						buffer.append(dissociation);
						if(!forDotLayout){							
							buffer.append(", pos=\"");
							buffer.append(st.getMIDDLEIntermediateVertex().getX());
							buffer.append(",");
							buffer.append(st.getMIDDLEIntermediateVertex().getY());
							buffer.append("\"");
						}						
						buffer.append("];\n");
						for(int j=0;j<dt.getReactants().size();j++){
							if(dt.getReactants().get(j).name.equals("Source/Sink")){
								buffer.append("  \"");
								Set keySet = tm.keySet();
								ArrayList<DotSourceSink> keyList = new ArrayList<DotSourceSink>(keySet);
								for(int k=0;k<keyList.size();k++){
								   if(tm.get(keyList.get(k)).equals(dt.getName())){
									   so=keyList.get(k);
									   if(so.getRole().equals("r")){
										   buffer.append(so.getId());
										   //tm.remove(keyList.get(k));
										   break;
									   }
								   }
								}
								buffer.append("\"");
							}else{
								buffer.append("  ");
								buffer.append("\"");
								buffer.append(dt.getReactants().get(j).name);
								buffer.append("\"");
							}
							buffer.append(" -> ");
							buffer.append("\"");
							buffer.append(ds);
							buffer.append("\"");
							buffer.append(" [arrowhead=none];\n");							
						}
						buffer.append("  \"");
						buffer.append(ds);	
						buffer.append("\"");
						buffer.append(" -> ");
						buffer.append("  \"");
						buffer.append(name);
						buffer.append("\"");
						buffer.append(" [arrowhead=none];\n");								
						for(int j=0;j<dt.getProducts().size();j++){
							buffer.append("  \"");
							buffer.append(name);
							buffer.append("\"");						
							buffer.append(" -> ");
							if(dt.getProducts().get(j).name.equals("Source/Sink")){
								buffer.append("  \"");
								Set keySet = tm.keySet();
								ArrayList<DotSourceSink> keyList = new ArrayList<DotSourceSink>(keySet);
								for(int k=0;k<keyList.size();k++){
								   if(tm.get(keyList.get(k)).equals(dt.getName())){
									   so=keyList.get(k);
									   if(so.getRole().equals("p")){
										   buffer.append(so.getId());
										   //tm.remove(keyList.get(k));
										   break;
									   }
								   }
								}
								buffer.append("\"");
							}else{
								buffer.append("  ");
								buffer.append("\"");
								buffer.append(dt.getProducts().get(j).name);
								buffer.append("\"");
							}
							
							buffer.append("\n");								
						}
						if(dt.modulators1!=null){
							for(int j=0;j<dt.getModulators1().size();j++){
								buffer.append("  \"");
								buffer.append(dt.getModulators1().get(j).name);
								buffer.append("\" -> ");
								buffer.append("\"");
								buffer.append(ds);
								buffer.append("\"");
								buffer.append(" [arrowhead=ediamond];\n");
							}	
						}
						if(dt.modulators2!=null){
							for(int j=0;j<dt.getModulators2().size();j++){	
								buffer.append("  \"");
								buffer.append(dt.getModulators2().get(j).name);
								buffer.append("\" -> ");
								buffer.append("\"");
								buffer.append(ds);
								buffer.append("\"");
								buffer.append(" [arrowhead=ediamond];\n");
							}	
						}
						
					}else if(rs[i] instanceof ReversibleAssociation){
						
						
						//Whats the first direction, reversAssoc or reverDissoc?
						int size=dt.getReactants().size();
						int size2=dt.getProducts().size();
						if(GraphUtilities.getCellByName(graph,dt.getReactants().get(0).getName()) instanceof EComplexCell){
							//if its reversibleDissociation.......
				
						
							//dissociation
							buffer.append(invisible);
							ReversibleAssociation st=(ReversibleAssociation)rs[i];	
							if(!forDotLayout){							
								buffer.append(", pos=\"");
								buffer.append(st.getIntermediateVertexRIGHT2().getX());
								buffer.append(",");
								buffer.append(st.getIntermediateVertexRIGHT2().getY());
								buffer.append("\"");
							}							
							buffer.append("];\n");
							buffer.append("  \"");
							String ds=UUID.randomUUID().toString().replaceAll("-","");
							buffer.append(ds);
							buffer.append("\"");
							buffer.append(dissociation);
							if(!forDotLayout){							
								buffer.append(", pos=\"");
								buffer.append(st.getIntermediateVertexMIDDLE2().getX());
								buffer.append(",");
								buffer.append(st.getIntermediateVertexMIDDLE2().getY());
								buffer.append("\"");
							}								
							buffer.append("];\n");
							
							for(int j=0;j<dt.getReactants().size();j++){
								if(dt.getReactants().get(j).name.equals("Source/Sink")){
									buffer.append("  \"");
									Set keySet = tm.keySet();
									ArrayList<DotSourceSink> keyList = new ArrayList<DotSourceSink>(keySet);
									for(int k=0;k<keyList.size();k++){
									   if(tm.get(keyList.get(k)).equals(dt.getName())){
										   so=keyList.get(k);
										   if(so.getRole().equals("r")){
											   buffer.append(so.getId());
											   //tm.remove(keyList.get(k));
											   break;
										   }
									   }
									}
									buffer.append("\"");
								}else{
									buffer.append("  ");
									buffer.append("\"");
									buffer.append(dt.getReactants().get(j).name);
									buffer.append("\"");
								}
								buffer.append(" -> ");
								buffer.append("\"");
								buffer.append(ds);
								buffer.append("\"");
								buffer.append(" [arrowhead=none];\n");							
							}
							buffer.append("  \"");
							buffer.append(ds);	
							buffer.append("\"");
							buffer.append(" -> ");
							buffer.append("  \"");
							buffer.append(name);
							buffer.append("\"");
							buffer.append(" [arrowhead=none];\n");								
							for(int j=0;j<dt.getProducts().size();j++){
								buffer.append("  \"");
								buffer.append(name);
								buffer.append("\"");						
								buffer.append(" -> ");
								if(dt.getProducts().get(j).name.equals("Source/Sink")){
									buffer.append("  \"");
									Set keySet = tm.keySet();
									ArrayList<DotSourceSink> keyList = new ArrayList<DotSourceSink>(keySet);
									for(int k=0;k<keyList.size();k++){
									   if(tm.get(keyList.get(k)).equals(dt.getName())){
										   so=keyList.get(k);
										   if(so.getRole().equals("p")){
											   buffer.append(so.getId());
											  //tm.remove(keyList.get(k));
											   break;
										   }
									   }
									}
									buffer.append("\"");
								}else{
									buffer.append("  ");
									buffer.append("\"");
									buffer.append(dt.getProducts().get(j).name);
									buffer.append("\"");
								}
								
								buffer.append("\n");								
							}
							if(dt.modulators1!=null){
								for(int j=0;j<dt.getModulators1().size();j++){
									buffer.append("  \"");
									buffer.append(dt.getModulators1().get(j).name);
									buffer.append("\" -> ");
									buffer.append("\"");
									buffer.append(ds);
									buffer.append("\"");
									buffer.append(" [arrowhead=ediamond];\n");
								}	
							}							
							//association
							
							buffer.append("  \"");
							String name2=UUID.randomUUID().toString().replaceAll("-","");
							buffer.append(name2);
							buffer.append("\"");
							buffer.append(invisible);
							if(!forDotLayout){							
								buffer.append(", pos=\"");
								buffer.append(st.getIntermediateVertexLEFT1().getX());
								buffer.append(",");
								buffer.append(st.getIntermediateVertexLEFT1().getY());
								buffer.append("\"");
							}								
							buffer.append("];\n");
							buffer.append("  \"");
							String as=UUID.randomUUID().toString().replaceAll("-","");
							buffer.append(as);
							buffer.append("\"");
							buffer.append(association);
							if(!forDotLayout){							
								buffer.append(", pos=\"");
								buffer.append(st.getIntermediateVertexMIDDLE1().getX());
								buffer.append(",");
								buffer.append(st.getIntermediateVertexMIDDLE1().getY());
								buffer.append("\"");
							}								
							buffer.append("];\n");
							
							for(int j=0;j<dt.getProducts().size();j++){
								if(dt.getProducts().get(j).name.equals("Source/Sink")){
									buffer.append("  \"");
									Set keySet = tm.keySet();
									ArrayList<DotSourceSink> keyList = new ArrayList<DotSourceSink>(keySet);
									for(int k=0;k<keyList.size();k++){
									   if(tm.get(keyList.get(k)).equals(dt.getName())){
										   so=keyList.get(k);
										   if(so.getRole().equals("p")){
											   buffer.append(so.getId());
											   //tm.remove(keyList.get(k));
											   break;
										   }
									   }
									}
									buffer.append("\"");
								}else{
									buffer.append("  ");
									buffer.append("\"");
									buffer.append(dt.getProducts().get(j).name);
									buffer.append("\"");
								}
								buffer.append(" -> ");
								buffer.append("  \"");
								buffer.append(name2);
								buffer.append("\"");
								buffer.append(" [arrowhead=none];\n");							
							}
							buffer.append("  \"");
							buffer.append(name2);
							buffer.append("\"");						
							buffer.append(" -> ");
							buffer.append("\"");
							buffer.append(as);
							buffer.append("\"");
							buffer.append(" [arrowhead=none];\n");
							
							for(int j=0;j<dt.getReactants().size();j++){
								buffer.append("  \"");
								buffer.append(as);	
								buffer.append("\"");
								buffer.append(" -> ");
								if(dt.getReactants().get(j).name.equals("Source/Sink")){
									buffer.append("  \"");
									Set keySet = tm.keySet();
									ArrayList<DotSourceSink> keyList = new ArrayList<DotSourceSink>(keySet);
									for(int k=0;k<keyList.size();k++){
									   if(tm.get(keyList.get(k)).equals(dt.getName())){
										   so=keyList.get(k);
										   if(so.getRole().equals("r")){
											   buffer.append(so.getId());
											   //tm.remove(keyList.get(k));
											   break;
										   }
									   }
									}
									buffer.append("\"");
								}else{
									buffer.append("  ");
									buffer.append("\"");
									buffer.append(dt.getReactants().get(j).name);
									buffer.append("\"");
								}
								buffer.append("\n");								
							}
							
							if(dt.modulators2!=null){
								for(int j=0;j<dt.getModulators2().size();j++){		
									buffer.append("  \"");
									buffer.append(dt.getModulators2().get(j).name);
									buffer.append("\" -> ");
									buffer.append("\"");
									buffer.append(as);
									buffer.append("\"");
									buffer.append(" [arrowhead=ediamond];\n");
								}	
							}							
							
							
							
						
						}else{							
							//if its reversibleAssociation.......
							
							
							//association
							buffer.append(invisible);
							ReversibleAssociation st=(ReversibleAssociation)rs[i];		
							if(!forDotLayout){							
								buffer.append(", pos=\"");
								buffer.append(st.getIntermediateVertexLEFT1().getX());
								buffer.append(",");
								buffer.append(st.getIntermediateVertexLEFT1().getY());
								buffer.append("\"");
							}							
							buffer.append("];\n");
							buffer.append("  \"");
							String as=UUID.randomUUID().toString().replaceAll("-","");
							buffer.append(as);
							buffer.append("\"");
							buffer.append(association);
							if(!forDotLayout){							
								buffer.append(", pos=\"");
								buffer.append(st.getIntermediateVertexMIDDLE1().getX());
								buffer.append(",");
								buffer.append(st.getIntermediateVertexMIDDLE1().getY());
								buffer.append("\"");
							}							
							buffer.append("];\n");
							
							for(int j=0;j<dt.getReactants().size();j++){
								if(dt.getReactants().get(j).name.equals("Source/Sink")){
									buffer.append("  \"");
									Set keySet = tm.keySet();
									ArrayList<DotSourceSink> keyList = new ArrayList<DotSourceSink>(keySet);
									for(int k=0;k<keyList.size();k++){
									   if(tm.get(keyList.get(k)).equals(dt.getName())){
										   so=keyList.get(k);
										   if(so.getRole().equals("r")){
											   buffer.append(so.getId());
											   //tm.remove(keyList.get(k));
											   break;
										   }
									   }
									}
									buffer.append("\"");
								}else{
									buffer.append("  ");
									buffer.append("\"");
									buffer.append(dt.getReactants().get(j).name);
									buffer.append("\"");
								}
								buffer.append(" -> ");
								buffer.append("  \"");
								buffer.append(name);
								buffer.append("\"");
								buffer.append(" [arrowhead=none];\n");							
							}
							buffer.append("  \"");
							buffer.append(name);
							buffer.append("\"");						
							buffer.append(" -> ");
							buffer.append("\"");
							buffer.append(as);
							buffer.append("\"");
							buffer.append(" [arrowhead=none];\n");
							
							for(int j=0;j<dt.getProducts().size();j++){
								buffer.append("  \"");
								buffer.append(as);	
								buffer.append("\"");
								buffer.append(" -> ");
								if(dt.getProducts().get(j).name.equals("Source/Sink")){
									buffer.append("  \"");
									Set keySet = tm.keySet();
									ArrayList<DotSourceSink> keyList = new ArrayList<DotSourceSink>(keySet);
									for(int k=0;k<keyList.size();k++){
									   if(tm.get(keyList.get(k)).equals(dt.getName())){
										   so=keyList.get(k);
										   if(so.getRole().equals("p")){
											   buffer.append(so.getId());
											   //tm.remove(keyList.get(k));
											   break;
										   }
									   }
									}
									buffer.append("\"");
								}else{
									buffer.append("  ");
									buffer.append("\"");
									buffer.append(dt.getProducts().get(j).name);
									buffer.append("\"");
								}
								buffer.append("\n");								
							}
							
							if(dt.modulators1!=null){
								for(int j=0;j<dt.getModulators1().size();j++){		
									buffer.append("  \"");
									buffer.append(dt.getModulators1().get(j).name);
									buffer.append("\" -> ");
									buffer.append("\"");
									buffer.append(as);
									buffer.append("\"");
									buffer.append(" [arrowhead=ediamond];\n");
								}	
							}							
							//disociation
							
							buffer.append("  \"");
							String name2=UUID.randomUUID().toString().replaceAll("-","");
							buffer.append(name2);
							buffer.append("\"");
							buffer.append(invisible);
							if(!forDotLayout){							
								buffer.append(", pos=\"");
								buffer.append(st.getIntermediateVertexRIGHT2().getX());
								buffer.append(",");
								buffer.append(st.getIntermediateVertexRIGHT2().getY());
								buffer.append("\"");
							}								
							buffer.append("];\n");
							buffer.append("  \"");
							String ds=UUID.randomUUID().toString().replaceAll("-","");
							buffer.append(ds);
							buffer.append("\"");
							buffer.append(dissociation);
							if(!forDotLayout){							
								buffer.append(", pos=\"");
								buffer.append(st.getIntermediateVertexMIDDLE2().getX());
								buffer.append(",");
								buffer.append(st.getIntermediateVertexMIDDLE2().getY());
								buffer.append("\"");
							}								
							buffer.append("];\n");
							
							for(int j=0;j<dt.getProducts().size();j++){
								if(dt.getProducts().get(j).name.equals("Source/Sink")){
									buffer.append("  \"");
									Set keySet = tm.keySet();
									ArrayList<DotSourceSink> keyList = new ArrayList<DotSourceSink>(keySet);
									for(int k=0;k<keyList.size();k++){
									   if(tm.get(keyList.get(k)).equals(dt.getName())){
										   so=keyList.get(k);
										   if(so.getRole().equals("p")){
											   buffer.append(so.getId());
											   //tm.remove(keyList.get(k));
											   break;
										   }
									   }
									}
									buffer.append("\"");
								}else{
									buffer.append("  ");
									buffer.append("\"");
									buffer.append(dt.getProducts().get(j).name);
									buffer.append("\"");
								}
								buffer.append(" -> ");
								buffer.append("\"");
								buffer.append(ds);
								buffer.append("\"");
								buffer.append(" [arrowhead=none];\n");							
							}
							buffer.append("  \"");
							buffer.append(ds);	
							buffer.append("\"");
							buffer.append(" -> ");
							buffer.append("  \"");
							buffer.append(name2);
							buffer.append("\"");
							buffer.append(" [arrowhead=none];\n");								
							for(int j=0;j<dt.getReactants().size();j++){
								buffer.append("  \"");
								buffer.append(name2);
								buffer.append("\"");							
								buffer.append(" -> ");
								if(dt.getReactants().get(j).name.equals("Source/Sink")){
									buffer.append("  \"");
									Set keySet = tm.keySet();
									ArrayList<DotSourceSink> keyList = new ArrayList<DotSourceSink>(keySet);
									for(int k=0;k<keyList.size();k++){
									   if(tm.get(keyList.get(k)).equals(dt.getName())){
										   so=keyList.get(k);
										   if(so.getRole().equals("r")){
											   buffer.append(so.getId());
											   //tm.remove(keyList.get(k));
											   break;
										   }
									   }
									}
									buffer.append("\"");
								}else{
									buffer.append("  ");
									buffer.append("\"");
									buffer.append(dt.getReactants().get(j).name);
									buffer.append("\"");
								}
								buffer.append("\n");								
							}						
							if(dt.modulators2!=null){
								for(int j=0;j<dt.getModulators2().size();j++){	
									buffer.append("  \"");
									buffer.append(dt.getModulators2().get(j).name);
									buffer.append("\" -> ");
									buffer.append("\"");
									buffer.append(ds);
									buffer.append("\"");
									buffer.append(" [arrowhead=ediamond];\n");
								}	
							}
							
						}
						
						
						
						
						
						
						
					}else if(rs[i] instanceof ReversibleStateTransition){
					
						//one direction.....
						
						buffer.append(invisible);
						ReversibleStateTransition st=(ReversibleStateTransition)rs[i];
						if(!forDotLayout){							
							buffer.append(", pos=\"");
							buffer.append(st.getIntermediateVertexLEFT1().getX());
							buffer.append(",");
							buffer.append(st.getIntermediateVertexLEFT1().getY());
							buffer.append("\"");
						}								
						buffer.append("];\n");
						buffer.append("  \"");
						String name2=UUID.randomUUID().toString().replaceAll("-","");
						buffer.append(name2);
						buffer.append("\"");
						buffer.append(stateTransition);
						if(!forDotLayout){							
							buffer.append(", pos=\"");
							buffer.append(st.getIntermediateVertexMIDDLE1().getX());
							buffer.append(",");
							buffer.append(st.getIntermediateVertexMIDDLE1().getY());
							buffer.append("\"");
						}						
						buffer.append("];\n");
						buffer.append("  \"");
						String name3=UUID.randomUUID().toString().replaceAll("-","");
						buffer.append(name3);
						buffer.append("\"");
						buffer.append(invisible);
						if(!forDotLayout){							
							buffer.append(", pos=\"");
							buffer.append(st.getIntermediateVertexRIGHT1().getX());
							buffer.append(",");
							buffer.append(st.getIntermediateVertexRIGHT1().getY());
							buffer.append("\"");
						}								
						buffer.append("];\n");
						
						for(int j=0;j<dt.getReactants().size();j++){
							if(dt.getReactants().get(j).name.equals("Source/Sink")){
								buffer.append("  \"");
								Set keySet = tm.keySet();
								ArrayList<DotSourceSink> keyList = new ArrayList<DotSourceSink>(keySet);
								for(int k=0;k<keyList.size();k++){
								   if(tm.get(keyList.get(k)).equals(dt.getName())){
									   so=keyList.get(k);
									   if(so.getRole().equals("r")){
										   buffer.append(so.getId());
										   //tm.remove(keyList.get(k));
										   break;
									   }
								   }
								}
								buffer.append("\"");
							}else{
								buffer.append("  ");
								buffer.append("\"");
								buffer.append(dt.getReactants().get(j).name);
								buffer.append("\"");
							}
							
							buffer.append(" -> ");
							buffer.append("\"");
							buffer.append(name);
							buffer.append("\"");
							buffer.append(" [arrowhead=none];\n");							
						}
						
						buffer.append("  \"");
						buffer.append(name);
						buffer.append("\"");
						buffer.append(" -> ");
						buffer.append("\"");
						buffer.append(name2);
						buffer.append("\"");
						buffer.append(" [arrowhead=none];\n");
						
						buffer.append("  \"");
						buffer.append(name2);
						buffer.append("\"");
						buffer.append(" -> ");
						buffer.append("\"");
						buffer.append(name3);
						buffer.append("\"");
						buffer.append(" [arrowhead=none];\n");
						
						for(int j=0;j<dt.getProducts().size();j++){
							buffer.append("  \"");
							buffer.append(name3);
							buffer.append("\"");							
							buffer.append(" -> ");
							if(dt.getProducts().get(j).name.equals("Source/Sink")){
								buffer.append("  \"");
								Set keySet = tm.keySet();
								ArrayList<DotSourceSink> keyList = new ArrayList<DotSourceSink>(keySet);
								for(int k=0;k<keyList.size();k++){
								   if(tm.get(keyList.get(k)).equals(dt.getName())){
									   so=keyList.get(k);
									   if(so.getRole().equals("p")){
										   buffer.append(so.getId());
										   //tm.remove(keyList.get(k));
										   break;
									   }
								   }
								}
								buffer.append("\"");
							}else{
								buffer.append("  ");
								buffer.append("\"");
								buffer.append(dt.getProducts().get(j).name);
								buffer.append("\"");
							}						
							buffer.append("\n");								
						}
						if(dt.modulators1!=null){
							for(int j=0;j<dt.getModulators1().size();j++){	
								buffer.append("  \"");								
								buffer.append(dt.getModulators1().get(j).name);
								buffer.append("\" -> ");
								buffer.append("  \"");
								buffer.append(name2);
								buffer.append("\"");
								buffer.append(" [arrowhead=ediamond];\n");
							}	
						}						
						String name4=UUID.randomUUID().toString().replaceAll("-","");
						buffer.append("  \"");
						buffer.append(name4);
						buffer.append("\"");
						buffer.append(invisible);
						if(!forDotLayout){							
							buffer.append(", pos=\"");
							buffer.append(st.getIntermediateVertexRIGHT2().getX());
							buffer.append(",");
							buffer.append(st.getIntermediateVertexRIGHT2().getY());
							buffer.append("\"");
						}								
						buffer.append("];\n");
						buffer.append("  \"");
						String name5=UUID.randomUUID().toString().replaceAll("-","");
						buffer.append(name5);
						buffer.append("\"");
						buffer.append(stateTransition);
						if(!forDotLayout){							
							buffer.append(", pos=\"");
							buffer.append(st.getIntermediateVertexMIDDLE2().getX());
							buffer.append(",");
							buffer.append(st.getIntermediateVertexMIDDLE2().getY());
							buffer.append("\"");
						}							
						buffer.append("];\n");
						buffer.append("  \"");
						String name6=UUID.randomUUID().toString().replaceAll("-","");
						buffer.append(name6);
						buffer.append("\"");
						buffer.append(invisible);
						if(!forDotLayout){							
							buffer.append(", pos=\"");
							buffer.append(st.getIntermediateVertexLEFT2().getX());
							buffer.append(",");
							buffer.append(st.getIntermediateVertexLEFT2().getY());
							buffer.append("\"");
						}								
						buffer.append("];\n");
						
						for(int j=0;j<dt.getProducts().size();j++){
							if(dt.getProducts().get(j).name.equals("Source/Sink")){
								buffer.append("  \"");
								Set keySet = tm.keySet();
								ArrayList<DotSourceSink> keyList = new ArrayList<DotSourceSink>(keySet);
								for(int k=0;k<keyList.size();k++){
								   if(tm.get(keyList.get(k)).equals(dt.getName())){
									   so=keyList.get(k);
									   if(so.getRole().equals("p")){
										   buffer.append(so.getId());
										   //tm.remove(keyList.get(k));
										   break;
									   }
								   }
								}
								buffer.append("\"");
							}else{
								buffer.append("  ");
								buffer.append("\"");
								buffer.append(dt.getProducts().get(j).name);
								buffer.append("\"");
							}
							
							buffer.append(" -> ");
							buffer.append("\"");
							buffer.append(name4);
							buffer.append("\"");
							buffer.append(" [arrowhead=none];\n");							
						}
						
						buffer.append("  \"");
						buffer.append(name4);
						buffer.append("\"");
						buffer.append(" -> ");
						buffer.append("\"");
						buffer.append(name5);
						buffer.append("\"");
						buffer.append(" [arrowhead=none];\n");						
						buffer.append("  \"");
						buffer.append(name5);
						buffer.append("\"");
						buffer.append(" -> ");
						buffer.append("\"");
						buffer.append(name6);
						buffer.append("\"");
						buffer.append(" [arrowhead=none];\n");
						
						for(int j=0;j<dt.getReactants().size();j++){
							buffer.append("  \"");
							buffer.append(name6);
							buffer.append("\"");							
							buffer.append(" -> ");
							if(dt.getReactants().get(j).name.equals("Source/Sink")){
								buffer.append("  \"");
								Set keySet = tm.keySet();
								ArrayList<DotSourceSink> keyList = new ArrayList<DotSourceSink>(keySet);
								for(int k=0;k<keyList.size();k++){
								   if(tm.get(keyList.get(k)).equals(dt.getName())){
									   so=keyList.get(k);
									   if(so.getRole().equals("r")){
										   buffer.append(so.getId());
										   //tm.remove(keyList.get(k));
										   break;
									   }
								   }
								}
								buffer.append("\"");
							}else{
								buffer.append("  ");
								buffer.append("\"");
								buffer.append(dt.getReactants().get(j).name);
								buffer.append("\"");
							}						
							buffer.append("\n");								
						}
						if(dt.modulators2!=null){
							for(int j=0;j<dt.getModulators2().size();j++){
								buffer.append("  \"");
								buffer.append(dt.getModulators2().get(j).name);
								buffer.append("\" -> ");
								buffer.append("  \"");
								buffer.append(name5);
								buffer.append("\"");
								buffer.append(" [arrowhead=ediamond];\n");
							}	
						}
					}
				}
				
				buffer.append("}");
				
				Writer output = null;				
				try {
					output = new BufferedWriter(new FileWriter(file));
					output.write(buffer.toString());
					output.close();
				} catch (IOException e) {
					e.printStackTrace();
				}				
				
				


				
				return null;
			}

			@Override
			public void finished() {
				// TODO Auto-generated method stub
				
			}};
		sw.start();
		
		
	}
	
}










