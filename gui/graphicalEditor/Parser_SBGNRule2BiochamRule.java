package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultGraphCell;

import fr.inria.contraintes.biocham.utils.Utils;


import java.util.ArrayList;
import java.util.StringTokenizer;
import java.util.UUID;

public class Parser_SBGNRule2BiochamRule {

	
	String reactants, products, modulatorsSingle, modulatorsDouble, kinetics;
	static boolean reversibleSingle;
	static boolean doubleReversible;
	static boolean hasAtLeastOneModulator=false;
	static String reactionType;	
	static String biochamRule="";
	static BiochamGraph graph;	
	static boolean fromGraph=false;
	static boolean dontDraw=false;
	String leftSide, rightSide;
	private static int dople=0;
	static ArrayList<BiochamObject> sbgnReactants=new ArrayList<BiochamObject>();
	static ArrayList<BiochamObject> sbgnProducts=new ArrayList<BiochamObject>();
	static ArrayList<BiochamObject> sbgnModulators1=new ArrayList<BiochamObject>();
	static ArrayList<BiochamObject> sbgnModulators2=new ArrayList<BiochamObject>();
	static BiochamEdgeData eData;	
	
	public Parser_SBGNRule2BiochamRule(BiochamGraph g){
		graph=g;		
		sbgnReactants.clear();
		sbgnProducts.clear();
		sbgnModulators1.clear();
		sbgnModulators2.clear();
		reversibleSingle=false;
		doubleReversible=false;
		hasAtLeastOneModulator=false;
		reactionType="";	
		biochamRule="";			
		eData=new BiochamEdgeData(graph);
	}
	
	public String parse(){
						
		if(kinetics!=null && kinetics!=""){
			if(!doubleReversible){
				biochamRule=kinetics+" for ";
			}			
			eData.setKinetics(kinetics);
		}
		
		ArrayList<String> elems=new ArrayList<String>();
		StringTokenizer st=new StringTokenizer(reactants," ");
   	  	while(st.hasMoreTokens()){
   		  elems.add(st.nextToken());
   	  	}
   	  	for(int i=0;i<elems.size();i++){   	  		
   	  			String part=elems.get(i).trim();   	  			
   	  			if(part.equals("Source/Sink")){
   	  				BiochamEntityData data=new BiochamEntityData(graph);
   	  				data.setName("Source/Sink");
   	  				ESourceSink sourceSink=new ESourceSink(data);				
   	  				graph.getGraphLayoutCache().insert(sourceSink);
   	  				//graph.refresh();
   	  				sbgnReactants.add(new BiochamObject(sourceSink.addPort(),0,"Source/Sink",sourceSink));	
   	  				//biochamRule+="_";
   	  				break;
   	  			}else{
   	  				String name="";
   	  				int stoich=0;
   	  				if(part.contains(";")){
   	  					name=part.substring(1,part.indexOf(";"));
   	  					stoich=Integer.valueOf(part.substring(part.indexOf(";")+1,part.length()-1));
   	  				}else{
   	  					name=part;
   	  					stoich=0;
   	  				
   	  				}
   	  				DefaultGraphCell cell=GraphUtilities.getCellByName(graph,name);	  
   	  				if(cell!=null){
   	  					Object port=cell.addPort();
   	  					BiochamEntityData dt=(BiochamEntityData)cell.getUserObject();
   	  					if(cell instanceof ENucleicAcidFeatureCell){
   	  						if(!name.startsWith("#")){   	  						
   	  							name="#"+name;   	
   	  							dt.setName(name);   	  						
   	  						}
   	  					}
   	  					cell.setUserObject(dt);   	  				
   	  					if(stoich>1){
   	  						//biochamRule+=stoich+"*"+name;   	  					
   	  						dt.setStoichoimetryForReaction(Integer.toString(stoich));
   	  					}     	  				
   	  					cell.setUserObject(dt);   	  					
   	  					sbgnReactants.add(new BiochamObject(port,stoich,name,cell));
   	  				}else{
   	  					Utils.debugMsg("CANNOT      FIND         CELL      "+name+"................");
   	  				}
   	  				
   	  			}
   	  			part=null;
   	  			Utils.debugMsg(elems.size());
   	  			
   	  	}
   	  	elems.clear();
   	  	
   	  	  	  	
   	  	if(modulatorsSingle!=null){
	   	  	st=new StringTokenizer(modulatorsSingle," ");
		  	while(st.hasMoreTokens()){
		  		elems.add(st.nextToken());
		  	}	  
		  	for(int i=0;i<elems.size();i++){	  	
		  			String name=elems.get(i).trim();
		  			//String name=part.substring(1,part.indexOf(";")); 				
	  				DefaultGraphCell cell=GraphUtilities.getCellByName(graph,name);	    				  
	  				Object port=cell.addPort();
	  				BiochamEntityData dt=(BiochamEntityData)cell.getUserObject();
	  				if(cell instanceof ENucleicAcidFeatureCell){
	  					if(!name.startsWith("#")){   	  						
	  						name="#"+name;   	
	  						dt.setName(name);   	  						
	  					}
	  				}  				  							
	  				cell.setUserObject(dt);
	  				sbgnModulators1.add(new BiochamObject(port,1,name,cell));
	  				hasAtLeastOneModulator=true;
		  			//part=null;
		  			name=null;	  		
		  	}		  	
   	  	}
   	  	
   	  	elems.clear();
   	  	if(modulatorsDouble!=null){
	   	  	st=new StringTokenizer(modulatorsDouble," ");
		  	while(st.hasMoreTokens()){
		  		elems.add(st.nextToken());
		  	}	  
		  	for(int i=0;i<elems.size();i++){	  	
		  			String name=elems.get(i).trim();
		  			//String name=part.substring(1,part.indexOf(";")); 				
	  				DefaultGraphCell cell=GraphUtilities.getCellByName(graph,name);	    				  
	  				Object port=cell.addPort();
	  				BiochamEntityData dt=(BiochamEntityData)cell.getUserObject();
	  				if(cell instanceof ENucleicAcidFeatureCell){
	  					if(!name.startsWith("#")){   	  						
	  						name="#"+name;   	
	  						dt.setName(name);   	  						
	  					}
	  				}  				
	  				cell.setUserObject(dt);
	  				sbgnModulators2.add(new BiochamObject(port,1,name,cell));	  	
	  				hasAtLeastOneModulator=true;
		  			//part=null;
		  			name=null;	  		
		  	}		  	
   	  	}
   	  	
   	  	elems.clear();
   	  	if(!products.contains("Selected")){
	   	  	st=new StringTokenizer(products," ");
		  	while(st.hasMoreTokens()){
		  		elems.add(st.nextToken());
		  	}
		  	for(int i=0;i<elems.size();i++){	  	
		  			String part=elems.get(i).trim();	  			
		  			if(part.equals("Source/Sink")){
		  				BiochamEntityData data=new BiochamEntityData(graph);
		  				data.setName("Source/Sink");
		  				ESourceSink sourceSink=new ESourceSink(data);				
		  				graph.getGraphLayoutCache().insert(sourceSink);
		  				//graph.refresh();
		  				sbgnProducts.add(new BiochamObject(sourceSink.addPort(),0,"Source/Sink",sourceSink));
		  				/*if(!hasAtLeastOneModulator && !doubleReversible){
		  					if(reversibleSingle){
		  						biochamRule+="<=>";
		  					}else{
		  						biochamRule+="=>";
		  					}	  					
		  					biochamRule+="_";
		  				}	  		*/		
		  				break;
		  			}else{
		  				String name=part.substring(1,part.indexOf(";"));
		  				int stoich=Integer.valueOf(part.substring(part.indexOf(";")+1,part.length()-1));
		  				DefaultGraphCell cell=GraphUtilities.getCellByName(graph,name);	    				  
		  				Object port=cell.addPort();
		  				BiochamEntityData dt=(BiochamEntityData)cell.getUserObject();
		  				if(cell instanceof ENucleicAcidFeatureCell){
		  					if(!name.startsWith("#")){   	  						
		  						name="#"+name;   	
		  						dt.setName(name);   	  						
		  					}
		  				}
		  				/*if(stoich>1){
		  					if(!doubleReversible){
		  						biochamRule+=stoich+"*"+name;
		  					}
		  					dt.setStoichoimetryForReaction(Integer.toString(stoich));
		  				}else{
		  					if(!doubleReversible){
		  						biochamRule+=name;
		  					}
		  					stoich=1;
		  				}      	  		*/		
		  				cell.setUserObject(dt);
		  				sbgnProducts.add(new BiochamObject(port,stoich,name,cell));
		  			}
		  			part=null;	  			
		  			/*if(i<elems.size()-1){
		  				if(!doubleReversible){
		  					biochamRule+="+";
		  				}
		  			}*/
		  	}	  	
		  	elems.clear();
   	  	}else{
   	  		if(reactionType.equals("Association")){
   	  			
   	  			String representingName=((BiochamEntityData)sbgnReactants.get(0).getInstance().getUserObject()).getRepresentingName();
   	  			String representingName2=((BiochamEntityData)sbgnReactants.get(0).getInstance().getUserObject()).getRepresentingName();
   	  			String complexName=sbgnReactants.get(0).getParentName();
   	  			if(complexName.contains("(")){
   	  				complexName=complexName.substring(0,complexName.indexOf("("));
   	  			}
   	  			String name=sbgnReactants.get(0).getParentName();
   	  			if(name.contains("(")){
   	  				name=name.substring(0,name.indexOf("("));
	  			}
   	  			//String repName=((BiochamEntityData)sbgnReactants.get(0).getInstance().getUserObject()).getRepresentingName();
   	  			//multimerization
   	  			if(sbgnReactants.size()==1){
   	  				int stoich=sbgnReactants.get(0).getStoichiometry();
   	  				if(stoich>1){
   	  					for(int i=0;i<stoich-1;i++){
   	  						complexName+="-"+name;
   	  						//representingName+="-"+repName;
   	  					}
   	  				}   	  				
   	  				if(GraphUtilities.getCellType(sbgnReactants.get(0).getInstance()).contains("Complex")){
					  BiochamEntityData data=new BiochamEntityData(graph);
					  data.setMultimerCardinality(stoich);	  
					  data.setName(complexName);
					  data.setRepresentingName(representingName);
					  data.setContainingMolecules(((BiochamEntityData)sbgnReactants.get(0).getInstance().getUserObject()).getContainingMolecules());
					  EComplexCell cc=new EComplexCell(data);
					  sbgnProducts.add(new BiochamObject(cc.addPort(),1,complexName,cc));		    		  
		    		  graph.getGraphLayoutCache().insert(cc);
   	  				}else if(GraphUtilities.getCellType(sbgnReactants.get(0).getInstance()).contains("Nucleic")){
   				  	  BiochamEntityData data=new BiochamEntityData(graph);
					  data.setMultimerCardinality(stoich);					  
					  data.setName(complexName);
					  data.setRepresentingName(representingName);
					  ENucleicAcidFeatureCell cc=new ENucleicAcidFeatureCell(data);
					  sbgnProducts.add(new BiochamObject(cc.addPort(),1,complexName,cc));
		    		  graph.getGraphLayoutCache().insert(cc);
   	  				}else{
   	  				  BiochamEntityData data=new BiochamEntityData(graph);
   				  	  data.setMultimerCardinality(stoich);
					  data.setName(complexName);
					  data.setRepresentingName(representingName);
					  DefaultGraphCell cc=new EMacromoleculeCell(data);					
					  sbgnProducts.add(new BiochamObject(cc.addPort(),1,complexName,cc));
		    		  graph.getGraphLayoutCache().insert(cc);
   	  				}
   	  			}else{
   	  				if(sbgnReactants.get(0).getStoichiometry()>1){
   	  					for(int i=1;i<sbgnReactants.get(0).getStoichiometry();i++){
   	  						complexName+="-"+name;
   	  						representingName+="-"+representingName2;
   	  					}
   	  				}
   	  				for(int i=1;i<sbgnReactants.size();i++){
   	  					String nm=sbgnReactants.get(i).getParentName();
   	  					if(nm.contains("(")){
   	  						nm=nm.substring(0,nm.indexOf("("));
   	  					}
   	  					if(nm.contains("::")){
	  						nm=nm.substring(0,nm.indexOf(":"));
	  					}
   	  					complexName+="-"+nm;
	  					representingName+="-"+((BiochamEntityData)sbgnReactants.get(i).getInstance().getUserObject()).getRepresentingName();
   	  					if(sbgnReactants.get(i).getStoichiometry()>1){
   	  						for(int j=1;j<sbgnReactants.get(i).getStoichiometry();j++){
   	  							complexName+="-"+nm;
   	  							representingName+="-"+((BiochamEntityData)sbgnReactants.get(i).getInstance().getUserObject()).getRepresentingName();
   	  						}
   	  					}   	  					
   	  				}
   	  				DefaultGraphCell cl=GraphUtilities.getCellByName(graph,complexName);
   	  				if(cl!=null){
   	  					sbgnProducts.add(new BiochamObject(cl.addPort(),1,complexName,cl));
   	  				}else{
   	  					
   	  					ArrayList<ContainingMolecule> cMols=new ArrayList<ContainingMolecule>();  	  				
	   	  				for(int i=0;i<sbgnReactants.size();i++){  			
	   	  					
		    				  DefaultGraphCell cell=sbgnReactants.get(i).getInstance();
			    			  
		    				  if(cell!=null){
			    				
		    					  name=sbgnReactants.get(i).getParentName();
		    					  if(name.contains("(")){
		    						  name=name.substring(0,name.indexOf("("));
		    					  }
		    					  if(name.contains("::")){
		    						  name=name.substring(0,name.indexOf(":"));
		    					  }
		    					  String type=GraphUtilities.getCellType(cell);
			    				  BiochamEntityData data=(BiochamEntityData)cell.getUserObject();
			    				 
			    				  int k1=sbgnReactants.get(i).getStoichiometry();
			    				  int k2=data.getMultimerCardinality();	
			    				  ContainingMolecule cm=new ContainingMolecule(name,data.getMoleculeState(),k1*k2,data.isModulator(),type,data.getInitialConcentration(),data.getRepresentingName(),data.getId(),cell);
			    				  //cm.setCardinality(k2);
			    				  cm.setReactionStoichiometry(k1);
		    					  cMols.add(cm);
		    				  }
	   	  				}
	   	  				BiochamEntityData data=new BiochamEntityData(graph);
	   	  				data.setContainingMolecules(cMols);
	   	  				data.setName(complexName);	
	   	  				data.setRepresentingName(representingName);
	   	  				EComplexCell cc=new EComplexCell(data);
	   	  				sbgnProducts.add(new BiochamObject(cc.addPort(),1,complexName,cc));
	   	  				graph.getGraphLayoutCache().insert(cc);		    
   	  				}
   	  						
   	  			}
   	  		}else if(reactionType.contains("Dissociation")){
   	  			
   	  			//String name=sbgnReactants.get(0).getParentName();
   	  			BiochamEntityData data=(BiochamEntityData)sbgnReactants.get(0).getInstance().getUserObject();
   	  			int stoich=data.getMultimerCardinality();
   	  			if(data.getMultimerCardinality()>1){
   	  				String representationName=data.getRepresentingName();

   	  				//its classical dissociation
   	  				/*if(data.getMoleculeType().contains("Complex")){
   	  					
   	  				}
   	  				//its de-multimerization
   	  				else{
   	  					
   	  				}*/
   	  				DefaultGraphCell cll=GraphUtilities.getCellByName(graph,representationName); 				
   	  				if(cll!=null){
   	  					sbgnProducts.add(new BiochamObject(cll.addPort(),data.getMultimerCardinality(),representationName,cll));
   	  				}else{
   	  					
   	  					if(data.getMoleculeType()!=null){
   	  						BiochamEntityData newDt=new BiochamEntityData(data);
   	  						newDt.setRepresentingName(representationName);
   	  						newDt.setName(representationName);
							if(sbgnReactants.get(0).getInstance() instanceof ENucleicAcidFeatureCell){
								
								newDt.setMultimerCardinality(0);
								ENucleicAcidFeatureCell cc=new ENucleicAcidFeatureCell(newDt);								
								sbgnProducts.add(new BiochamObject(cc.addPort(),stoich,data.getRepresentingName(),cc));
								graph.getGraphLayoutCache().insert(cc);		
							}else if(sbgnReactants.get(0).getInstance() instanceof EComplexCell){
								
								newDt.setMultimerCardinality(0);
								EComplexCell cc=new EComplexCell(newDt);								
								sbgnProducts.add(new BiochamObject(cc.addPort(),stoich,data.getRepresentingName(),cc));
								graph.getGraphLayoutCache().insert(cc);	
							}else{
								
								newDt.setMultimerCardinality(0);
								EMacromoleculeCell cc=new EMacromoleculeCell(newDt);								
								sbgnProducts.add(new BiochamObject(cc.addPort(),stoich,data.getRepresentingName(),cc));
								graph.getGraphLayoutCache().insert(cc);		
							}
   	  					}
   	  				}
   	  			}else{
	   	  			ArrayList<ContainingMolecule> conMols=data.getContainingMolecules();;
	   	  			if(conMols!=null){
	   	  				for(int i=0;i<conMols.size();i++){   	  					
	   	  					if(conMols.get(i).getInstance()!=null){
	   	  						if(GraphUtilities.getCellByName(graph,conMols.get(i).getName())!=null){
	   	  							sbgnProducts.add(new BiochamObject(conMols.get(i).getInstance().addPort(),conMols.get(i).getReactionStoichiometry(),conMols.get(i).getName(),conMols.get(i).getInstance()));
	   	  						}else{
	   	  							BiochamEntityData dt1=new BiochamEntityData(graph,conMols.get(i));
	   	  							if(dt1.getMoleculeType()!=null){
	   	  								if(dt1.getMoleculeType().contains("Nucleic")){
	   	  									ENucleicAcidFeatureCell cc=new ENucleicAcidFeatureCell(dt1);
	   	  									conMols.get(i).setInstance(cc);	   	  									
	   	  									sbgnProducts.add(new BiochamObject(cc.addPort(),conMols.get(i).getReactionStoichiometry(),dt1.getName(),cc));
	   	  									graph.getGraphLayoutCache().insert(cc);		
	   	  								}else if(dt1.getMoleculeType().contains("Complex")){
	   	  									EComplexCell cc=new EComplexCell(dt1);   	  									
		  									conMols.get(i).setInstance(cc);
		  									sbgnProducts.add(new BiochamObject(cc.addPort(),conMols.get(i).getReactionStoichiometry(),dt1.getName(),cc));
		  									graph.getGraphLayoutCache().insert(cc);	
	   	  								}else{
	   	  									EMacromoleculeCell cc=new EMacromoleculeCell(dt1);
		  									conMols.get(i).setInstance(cc);
		  									sbgnProducts.add(new BiochamObject(cc.addPort(),conMols.get(i).getReactionStoichiometry(),dt1.getName(),cc));
		  									graph.getGraphLayoutCache().insert(cc);		
	   	  								}
	   	  							}
	   	  						}
	   	  						
	   	  						/*BiochamEntityData dt1=GraphUtilities.getBiochamEntityDataFromCell(conMols.get(i).getUserObject());
	   	  						if(dt1!=null){
	   	  							int genStoich=dt1.getMultimerCardinality();
	   	  							if(genStoich<1){
	   	  								genStoich=1;
	   	  							}
	   	  							int reactionStoich=sbgnReactants.get(0).getStoichiometry();
	   	  							if(reactionStoich>1){
	   	  								dt1.setStoichoimetryForReaction(reactionStoich);
	   	  							    genStoich*=reactionStoich;
	   	  							}   	  							
	   	  							sbgnProducts.add(new BiochamObject(conMols.get(i).getInstance().addPort(),genStoich,dt1.getName(),conMols.get(i).getInstance()));
	   	  						}
	   	  						dt1=null;*/
	   	  					} 					
	   	  				}
	   	  			}
	   	  			data=null;
	   	  			conMols=null;
   	  			}
   	  			
   	  			
   	  		}
   	  	}
   	  	
   	  	boolean anotherOption=(reactionType.contains("Dissociation") || reactionType.contains("Association")) && (reversibleSingle || doubleReversible); 
	  	if(doubleReversible || anotherOption){
	  		//it is double reversible
   	  		//create 2 biocham rules......
	  		
	  		
	  		
	  		String tempBcRule="";
	  		if(sbgnModulators1.size()>0){
	  			
	  			for(int i=0;i<sbgnReactants.size();i++){
	  				if(!sbgnReactants.get(i).getParentName().equals("Source/Sink")){
	  					if(sbgnReactants.get(i).getParentName().contains("(")){
	  						biochamRule+=sbgnReactants.get(i).getParentName().substring(0,sbgnReactants.get(i).getParentName().lastIndexOf("("))+"+";	
	  					}else{
	  						biochamRule+=sbgnReactants.get(i).getParentName()+"+";
	  					}
	  					
   	  				}	  				
	  			}	
	  			if(!(sbgnReactants.size()==1 && sbgnReactants.get(0).getParentName().equals("Source/Sink"))){
	  				if(!biochamRule.contains("for")){
	  					tempBcRule=biochamRule.substring(0,biochamRule.length()-1);
	  				}else{
	  					tempBcRule=biochamRule.substring(biochamRule.indexOf("for"),biochamRule.length()-1);
	  				}
	  			}	  			
   	  			for(int i=0;i<sbgnModulators1.size();i++){
   	  				if(sbgnModulators1.get(i).getParentName().contains("(")){
						biochamRule+=sbgnModulators1.get(i).getParentName().substring(0,sbgnModulators1.get(i).getParentName().lastIndexOf("("));	
					}else{
						biochamRule+=sbgnModulators1.get(i).getParentName();
					}   	  				
   	  				if(i<sbgnModulators1.size()-1){
	  					biochamRule+="+";
	  				}
   	  			}	
   	  			
   	  			biochamRule+="=>";
   	  			for(int i=0;i<sbgnProducts.size();i++){
   	  				if(!sbgnProducts.get(i).getParentName().equals("Source/Sink")){
   	  					if(sbgnProducts.get(i).getParentName().contains("(")){
   	  						biochamRule+=sbgnProducts.get(i).getParentName().substring(0,sbgnProducts.get(i).getParentName().lastIndexOf("("))+"+";	
   	  					}else{
   	  						biochamRule+=sbgnProducts.get(i).getParentName()+"+";
   	  					}   	  					
   	  				}
	  			}	
   	  			for(int i=0;i<sbgnModulators1.size();i++){
   	  				if(sbgnModulators1.get(i).getParentName().contains("(")){
 						biochamRule+=sbgnModulators1.get(i).getParentName().substring(0,sbgnModulators1.get(i).getParentName().lastIndexOf("("));
 					}else{
 						biochamRule+=sbgnModulators1.get(i).getParentName();
 					}  
   	  				
   	  				if(i<sbgnModulators1.size()-1){
   	  					biochamRule+="+";
   	  				}
   	  			}	
   	  		}else{
   	  			for(int i=0;i<sbgnReactants.size();i++){
	  				if(!sbgnReactants.get(i).getParentName().equals("Source/Sink")){
	  					if(sbgnReactants.get(i).getParentName().contains("(")){
   	  						biochamRule+=sbgnReactants.get(i).getParentName().substring(0,sbgnReactants.get(i).getParentName().lastIndexOf("("));	
   	  					}else{
   	  						biochamRule+=sbgnReactants.get(i).getParentName();
   	  					}  
	  					
	  				}else{
	  					biochamRule+="_";
	  					break;
	  				} 				
	  				if(i<sbgnReactants.size()-1){
	  					biochamRule+="+";
	  				}
	  			}
   	  			if(!biochamRule.contains("for")){
   	  				tempBcRule=biochamRule;
				}else{
					tempBcRule=biochamRule.substring(biochamRule.indexOf("for"));
				}
   	  			
   	  			biochamRule+="=>";
   	  			for(int i=0;i<sbgnProducts.size();i++){
   	  				if(!sbgnProducts.get(i).getParentName().equals("Source/Sink")){
   	  					if(sbgnProducts.get(i).getParentName().contains("(")){
	  						biochamRule+=sbgnProducts.get(i).getParentName().substring(0,sbgnProducts.get(i).getParentName().lastIndexOf("("));	
	  					}else{
	  						biochamRule+=sbgnProducts.get(i).getParentName();
	  					}   
   	  					
   	  				}else{
   	  					biochamRule+="_";
   	  					break;
   	  				} 				
   	  				if(i<sbgnProducts.size()-1){
   	  					biochamRule+="+";
   	  				}
   	  			}
   	  		}
	  		biochamRule="{"+biochamRule+",";
	  		//tempBcRule
	  		if(sbgnModulators2.size()>0){
	  			for(int i=0;i<sbgnProducts.size();i++){
	  				if(!sbgnProducts.get(i).getParentName().equals("Source/Sink")){
	  					if(sbgnProducts.get(i).getParentName().contains("(")){
	  						biochamRule+=sbgnProducts.get(i).getParentName().substring(0,sbgnProducts.get(i).getParentName().lastIndexOf("("))+"+";	
	  					}else{
	  						biochamRule+=sbgnProducts.get(i).getParentName()+"+";
	  					}  	  					
   	  				} 	
	  				
	  			}	
	  			for(int i=0;i<sbgnModulators2.size();i++){
	  				if(sbgnModulators2.get(i).getParentName().contains("(")){  						
  						biochamRule+=sbgnModulators2.get(i).getParentName().substring(0,sbgnModulators2.get(i).getParentName().lastIndexOf("("));
  					}else{
  						biochamRule+=sbgnModulators2.get(i).getParentName();
  					} 
   	  				
   	  				if(i<sbgnModulators2.size()-1){
   	  					biochamRule+="+";
   	  				}
   	  			}	
	  			biochamRule+="=>"+tempBcRule;
   	  			for(int i=0;i<sbgnModulators2.size();i++){
   	  				if(sbgnModulators2.get(i).getParentName().contains("(")){
						biochamRule+=sbgnModulators2.get(i).getParentName().substring(0,sbgnModulators2.get(i).getParentName().lastIndexOf("("));	
					}else{
						biochamRule+="+"+sbgnModulators2.get(i).getParentName();
					}  	
   	  				
   	  			}	  			
   	  		}else{   	  			
   	  			for(int i=0;i<sbgnProducts.size();i++){
   	  				if(!sbgnProducts.get(i).getParentName().equals("Source/Sink")){
   	  					if(sbgnProducts.get(i).getParentName().contains("(")){
   	  						biochamRule+=sbgnProducts.get(i).getParentName().substring(0,sbgnProducts.get(i).getParentName().lastIndexOf("("));	
   	  					}else{
   	  						biochamRule+=sbgnProducts.get(i).getParentName();
   	  					}  	
   	  					
	  				}else{
	  					biochamRule+="_";
	  					break;
	  				}    	  				
   	  				if(i<sbgnProducts.size()-1){
   	  					biochamRule+="+";
   	  				}
   	  			}
   	  			if(tempBcRule==""){
   	  				tempBcRule+="_";
   	  			}
   	  			biochamRule+="=>"+tempBcRule;
   	  		}
	  		biochamRule+="}";
	  	}else{
	  		//its single reversible or not at all...   	  		
   	  		if(sbgnModulators1.size()>0){
   	  			
   	  			
   	  			if(!(sbgnReactants.size()==1 && sbgnReactants.get(0).getParentName().equals("Source/Sink"))){
   	  				for(int i=0;i<sbgnReactants.size();i++){
   	  					if(sbgnReactants.get(i).getStoichiometry()>1){
   	  						if(sbgnReactants.get(i).getParentName().contains("(")){   	  							
   	  							biochamRule+=Integer.toString(sbgnReactants.get(i).getStoichiometry())+"*"+sbgnReactants.get(i).getParentName().substring(0,sbgnReactants.get(i).getParentName().lastIndexOf("("));
   	  						}else{
   	  							biochamRule+=Integer.toString(sbgnReactants.get(i).getStoichiometry())+"*"+sbgnReactants.get(i).getParentName();
   	  						}  	
   	  						
   	  					}else{
   	  						if(sbgnReactants.get(i).getParentName().contains("(")){
   	  							biochamRule+=sbgnReactants.get(i).getParentName().substring(0,sbgnReactants.get(i).getParentName().lastIndexOf("("));	
   	  						}else{
   	  							biochamRule+=sbgnReactants.get(i).getParentName();
   	  						} 
   	  						
   	  					}			
   	  					if(i<sbgnReactants.size()-1){
   	  						biochamRule+="+";
   	  					}
	  				}	
   	  				for(int i=0;i<sbgnModulators1.size();i++){
   	  					if(sbgnModulators1.get(i).getParentName().contains("(")){
 							biochamRule+="+"+sbgnModulators1.get(i).getParentName().substring(0,sbgnModulators1.get(i).getParentName().lastIndexOf("("));	
 						}else{
 							biochamRule+="+"+sbgnModulators1.get(i).getParentName();
 						} 
   	  					
   	  				}
   	  			}else{
   	  				for(int i=0;i<sbgnModulators1.size();i++){
   	  					if(sbgnModulators1.get(i).getParentName().contains("(")){
							biochamRule+=sbgnModulators1.get(i).getParentName().substring(0,sbgnModulators1.get(i).getParentName().lastIndexOf("("));	
						}else{
							biochamRule+=sbgnModulators1.get(i).getParentName();
						}    	  					
   	  					if(i<sbgnModulators1.size()-1){
   	  						biochamRule+="+";
   	  					}
   	  				}   	  				
   	  			}
   	  				
   	  			if(reversibleSingle){
   	  				//it is single reversible
   	  				biochamRule+="<=>";
   	  			}else{
   	  				biochamRule+="=>";
   	  			}
   	  			for(int i=0;i<sbgnProducts.size();i++){
   	  			if(!sbgnProducts.get(i).getParentName().equals("Source/Sink")){
   	  				if(sbgnProducts.get(i).getStoichiometry()>1){
   	  					if(sbgnProducts.get(i).getParentName().contains("(")){   	  						
   	  						biochamRule+=Integer.toString(sbgnProducts.get(i).getStoichiometry())+"*"+sbgnProducts.get(i).getParentName().substring(0,sbgnProducts.get(i).getParentName().lastIndexOf("("))+"+";
   	  					}else{
   	  						biochamRule+=Integer.toString(sbgnProducts.get(i).getStoichiometry())+"*"+sbgnProducts.get(i).getParentName()+"+";
   	  					}	  				
	  				}else{
	  					if(sbgnProducts.get(i).getParentName().contains("(")){								
							biochamRule+=sbgnProducts.get(i).getParentName().substring(0,sbgnProducts.get(i).getParentName().lastIndexOf("("))+"+";
						}else{
							biochamRule+=sbgnProducts.get(i).getParentName()+"+";
						} 
	  					
	  				}
  				}
   	  				
   	  			}	
   	  			for(int i=0;i<sbgnModulators1.size();i++){
   	  				if(sbgnModulators1.get(i).getParentName().contains("(")){
   	  					biochamRule+=sbgnModulators1.get(i).getParentName().substring(0,sbgnModulators1.get(i).getParentName().lastIndexOf("("));	
   	  				}else{
   	  					biochamRule+=sbgnModulators1.get(i).getParentName();
   	  				} 	  				
	  				if(i<sbgnModulators1.size()-1){
   	  					biochamRule+="+";
   	  				}
	  			}	
   	  		}else{
   	  			//biochamRule="";
   	  			if(sbgnReactants.size()==1 && sbgnReactants.get(0).getParentName().equals("Source/Sink")){
	  				biochamRule+="_";
	  			}else{
	  				for(int i=0;i<sbgnReactants.size();i++){
	  					if(sbgnReactants.get(i).getStoichiometry()>1){
	  						//System.out.println(sbgnReactants.get(i).getParentName());
	  						if(sbgnReactants.get(i).getParentName().contains("(")){	  	   	  					
	  	   	  					biochamRule+=Integer.toString(sbgnReactants.get(i).getStoichiometry())+"*"+sbgnReactants.get(i).getParentName().substring(0,sbgnReactants.get(i).getParentName().lastIndexOf("("));
	  	   	  				}else{
	  	   	  					biochamRule+=Integer.toString(sbgnReactants.get(i).getStoichiometry())+"*"+sbgnReactants.get(i).getParentName();
	  	   	  				} 	 
   	  						
   	  					}else{
   	  						if(sbgnReactants.get(i).getParentName().contains("(")){
   	  							biochamRule+=sbgnReactants.get(i).getParentName().substring(0,sbgnReactants.get(i).getParentName().lastIndexOf("("));	
   	  						}else{
   	  							biochamRule+=sbgnReactants.get(i).getParentName();
   	  						} 	 
   	  						
   	  					}			
		  				if(i<sbgnReactants.size()-1){
	   	  					biochamRule+="+";
	   	  				}
		  			}	
	  			}
   	  			if(reversibleSingle){
	  				//it is single reversible
	  				biochamRule+="<=>";
	  			}else{
	  				biochamRule+="=>";
	  			}
   	  			for(int i=0;i<sbgnProducts.size();i++){
   	  				if(!sbgnProducts.get(i).getParentName().equals("Source/Sink")){
   	  					if(sbgnProducts.get(i).getStoichiometry()>1){
   	  						if(sbgnProducts.get(i).getParentName().contains("(")){	  								
	  							biochamRule+=Integer.toString(sbgnProducts.get(i).getStoichiometry())+"*"+sbgnProducts.get(i).getParentName().substring(0,sbgnProducts.get(i).getParentName().lastIndexOf("("));
	  						}else{
	  							biochamRule+=Integer.toString(sbgnProducts.get(i).getStoichiometry())+"*"+sbgnProducts.get(i).getParentName();
	  						}    	  						
   	  					}else{
   	  						if(sbgnProducts.get(i).getParentName().contains("(")){
	  							biochamRule+=sbgnProducts.get(i).getParentName().substring(0,sbgnProducts.get(i).getParentName().lastIndexOf("("));	
	  						}else{
	  							biochamRule+=sbgnProducts.get(i).getParentName();
	  						} 
   	  						
   	  					}
   	  				}else{
   	  					biochamRule+="_";
   	  				}				
	  				if(i<sbgnProducts.size()-1){
   	  					biochamRule+="+";
   	  				}
	  			}	
   	  		}
	  	}
		return biochamRule;
	}
	
	
	public static void drawRule() {
		
		
		GraphUtilities.setReactants(eData, sbgnReactants);		
		GraphUtilities.setProducts(eData,sbgnProducts);
		GraphUtilities.setModulators1(eData,sbgnModulators1);		
		eData.setReversible(reversibleSingle);
		ArrayList<UUID> molecules=new ArrayList<UUID>();
		
		for(int i=0;i<sbgnProducts.size();i++){
			molecules.add(((BiochamEntityData)sbgnProducts.get(i).getInstance().getUserObject()).getId());
		}
		for(int i=0;i<sbgnReactants.size();i++){
			molecules.add(((BiochamEntityData)sbgnReactants.get(i).getInstance().getUserObject()).getId());
		} 
		if(sbgnModulators1.size()>0){
			for(int i=0;i<sbgnModulators1.size();i++){
				molecules.add(((BiochamEntityData)sbgnModulators1.get(i).getInstance().getUserObject()).getId());
			}
		}
		if(sbgnModulators2.size()>0){
			for(int i=0;i<sbgnModulators2.size();i++){
				molecules.add(((BiochamEntityData)sbgnModulators2.get(i).getInstance().getUserObject()).getId());
			}
		}
  	  	eData.setMolecules(molecules);
  	  	eData.setType(reactionType);  	  
		DefaultGraphCell st;			
		
	
		if(reactionType.contains("Association")){
			
			if(doubleReversible || reversibleSingle){
				setDople(2);
				GraphUtilities.setModulators2(eData,sbgnModulators2);
				st=new ReversibleAssociation(eData,sbgnReactants,sbgnProducts.get(0),null,graph,biochamRule);
				((ReversibleAssociation)st).setReversibleDissociation(false);
				if(hasAtLeastOneModulator){
					if(sbgnModulators1!=null){
						for(int i=0;i<sbgnModulators1.size();i++){
							((ReversibleAssociation)st).addModulator1(sbgnModulators1.get(i).getParentName(),1, false);
						}
					}
					if(sbgnModulators2!=null){
						for(int i=0;i<sbgnModulators2.size();i++){
							((ReversibleAssociation)st).addModulator2(sbgnModulators2.get(i).getParentName(),1, false);
						}
					}
				}
			}else{
				st=new Association(eData,sbgnReactants,sbgnProducts.get(0),null,graph,biochamRule);
				if(hasAtLeastOneModulator){
					if(sbgnModulators1!=null){
						for(int i=0;i<sbgnModulators1.size();i++){
							((Association)st).addModulator(sbgnModulators1.get(i).getParentName(),1, false);
						}
					}
				}
			}		
			
			
		}else if(reactionType.contains("Dissociation")){
				
			if(doubleReversible || reversibleSingle){
				setDople(2);
				GraphUtilities.setModulators2(eData,sbgnModulators2);
				st=new ReversibleAssociation(eData,sbgnProducts,sbgnReactants.get(0),null,graph,biochamRule);	
				((ReversibleAssociation)st).setReversibleDissociation(true);
				if(hasAtLeastOneModulator){
				
					if(sbgnModulators1!=null){
						for(int i=0;i<sbgnModulators1.size();i++){
							((ReversibleAssociation)st).addModulator2(sbgnModulators1.get(i).getParentName(),1, false);
						}
					}
					if(sbgnModulators2!=null){
						for(int i=0;i<sbgnModulators2.size();i++){
							((ReversibleAssociation)st).addModulator1(sbgnModulators2.get(i).getParentName(),1, false);
						}
					}
				}
			}else{
				st=new Dissociation(eData,sbgnReactants.get(0),sbgnProducts,null,graph,biochamRule);
				if(hasAtLeastOneModulator){
					if(sbgnModulators1!=null){
						for(int i=0;i<sbgnModulators1.size();i++){
							((Dissociation)st).addModulator(sbgnModulators1.get(i).getParentName(),1, false);
						}
					}
				}
			}		
			
			
		}else{
			
			if(doubleReversible){
				
				setDople(2);
				GraphUtilities.setModulators2(eData,sbgnModulators2);
				st=new ReversibleStateTransition(eData,sbgnReactants,sbgnProducts,null,null,graph,true,biochamRule);	
				
				if(hasAtLeastOneModulator){
				
					if(sbgnModulators1.size()>0){
						for(int i=0;i<sbgnModulators1.size();i++){
							((ReversibleStateTransition)st).addModulator1(sbgnModulators1.get(i).getParentName(),1, false);
						}
					}
					if(sbgnModulators2.size()>0){
						for(int i=0;i<sbgnModulators2.size();i++){
							((ReversibleStateTransition)st).addModulator2(sbgnModulators2.get(i).getParentName(),1, false);
						}
					}
				}
			}else{
				st=new StateTransition(eData,sbgnReactants,sbgnProducts,null,null,graph,reversibleSingle,biochamRule);
				if(hasAtLeastOneModulator){
					if(sbgnModulators1!=null){
						for(int i=0;i<sbgnModulators1.size();i++){
							((StateTransition)st).addModulator(sbgnModulators1.get(i).getParentName(),1, false);
						}
					}
				}
			}		
		}
		
		
		 
	}
	
	
	
	
	
	public Boolean getDoubleReversible() {
		return doubleReversible;
	}

	public void setDoubleReversible(Boolean doubleReversible) {
		this.doubleReversible = doubleReversible;
	}

	public String getKinetics() {
		return kinetics;
	}

	public void setKinetics(String kinetics) {
		this.kinetics = kinetics;
	}

	public String getModulatorsDouble() {
		return modulatorsDouble;
	}

	public void setModulatorsDouble(String modulatorsDouble) {
		this.modulatorsDouble = modulatorsDouble;
	}

	public String getModulatorsSingle() {
		return modulatorsSingle;
	}

	public void setModulatorsSingle(String modulatorsSingle) {
		this.modulatorsSingle = modulatorsSingle;
	}

	public String getProducts() {
		return products;
	}

	public void setProducts(String products) {
		this.products = products;
	}

	public String getReactants() {
		return reactants;
	}

	public void setReactants(String reactants) {
		this.reactants = reactants;
	}

	public String getReactionType() {
		return reactionType;
	}

	public void setReactionType(String reactionType) {
		this.reactionType = reactionType;
	}

	public Boolean getReversibleSingle() {
		return reversibleSingle;
	}

	public void setReversibleSingle(Boolean reversibleSingle) {
		this.reversibleSingle = reversibleSingle;
	}

	public static boolean isFromGraph() {
		return fromGraph;
	}

	public static void setFromGraph(boolean fromGraph) {
		Parser_SBGNRule2BiochamRule.fromGraph = fromGraph;
	}

	public static boolean isDontDraw() {
		return dontDraw;
	}

	public static void setDontDraw(boolean dontDraw) {
		Parser_SBGNRule2BiochamRule.dontDraw = dontDraw;
	}

	public static void setDople(int dople) {
		Parser_SBGNRule2BiochamRule.dople = dople;
	}

	public static int getDople() {
		return dople;
	}



	
}
