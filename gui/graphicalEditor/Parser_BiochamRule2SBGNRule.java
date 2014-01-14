package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.DefaultGraphCell;

import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.utils.Utils;

import java.util.ArrayList;
import java.util.StringTokenizer;
import java.util.UUID;





public class Parser_BiochamRule2SBGNRule {

	
	
	
	BiochamModel model;
	String reactionName;
	String kinetics;
	String reactants, products, modulators;
	ArrayList<String> leftSolutions,rightSolutions;
	boolean reversible=false;

	public Parser_BiochamRule2SBGNRule(BiochamModel bcModel){
		model=bcModel;
	}
	

	/**
	 * 
	 * 
	 * reactionName : kinetics for basic_reaction
	 * 
	 * 
	 * basic_reaction: solution =>/=[object]=>/=[S=>S]=>/<=>/<=[object]=>.
	 * 
	 * 
	 * solution: _/object/int*object/solution+solution/(solution)
	 * 
	 * 
	 * **/
	public void parse(String oRule){
		
		String rule=oRule;
		String ch=rule.substring(rule.indexOf(":")+1,rule.indexOf(":")+2);
		if(rule.contains(":") && !rule.substring(rule.indexOf(":")+1,rule.indexOf(":")+2).equals(":")){
			reactionName=rule.substring(0,rule.indexOf(":"));
			rule=rule.substring(rule.indexOf(":")+1);
		}
		if(rule.contains("for")){
			kinetics=rule.substring(0,rule.indexOf("for")).trim();
			rule=rule.substring(rule.indexOf("for")+3).trim();
		}
		
		String leftSide="", middle="",rightSide="";
		if(rule.contains("<=")){
			leftSide=rule.substring(0,rule.indexOf("<="));
			middle=rule.substring(rule.indexOf("<=")+2,rule.lastIndexOf(">"));
			reversible=true;
		}else{
			leftSide=rule.substring(0,rule.indexOf("="));
			middle=rule.substring(rule.indexOf("=")+1,rule.lastIndexOf(">"));
		}
		rightSide=rule.substring(rule.lastIndexOf("=>")+2);
		
		/**
		 * 
		 * LEFT SIDE*
		 *
		 * 
		 **/		
		leftSolutions=new ArrayList<String>();
		/**there is more then 1 solution**/
		if(leftSide.contains("+")){
			StringTokenizer st=new StringTokenizer(leftSide,"+");
			while(st.hasMoreTokens()){
				String sl=st.nextToken();
				if(sl.startsWith("(") && sl.endsWith(")")){
					leftSolutions.add(sl.substring(1,sl.length()-1).trim());	
				}else{
					leftSolutions.add(sl.trim());
				}	
				sl=null;
			}
			st=null;
		}
		/**there is only 1 solution**/
		else{
			if(leftSide.startsWith("(") && leftSide.endsWith(")")){
				leftSide=leftSide.substring(1,leftSide.lastIndexOf(")"));				
			}
			leftSolutions.add(leftSide.trim());
		}
		

		/**
		 * 
		 * RIGHT SIDE*
		 *
		 * 
		 **/	
		rightSolutions=new ArrayList<String>();
		/**there is more then 1 solution**/
		if(rightSide.contains("+")){
			StringTokenizer st=new StringTokenizer(rightSide,"+");
			while(st.hasMoreTokens()){
				String sl=st.nextToken();
				if(sl.startsWith("(") && sl.endsWith(")")){
					rightSolutions.add(sl.substring(1,sl.length()-1).trim());	
				}else{
					rightSolutions.add(sl.trim());
				}	
				sl=null;
			}
			st=null;
		}
		/**there is only 1 solution**/
		else{
			if(rightSide.startsWith("(") && rightSide.endsWith(")")){
				rightSide=rightSide.substring(1,rightSide.lastIndexOf(")"));				
			}
			rightSolutions.add(rightSide.trim());
		}
		
		
		/**
		 * 
		 * MIDDLE SIDE
		 *
		 * 
		 **/
		
		/**..It means there is something, its not empty middle....**/
		if(middle.endsWith("=")){		
			
			ArrayList<String> lS=new ArrayList<String>();
			ArrayList<String> rS=new ArrayList<String>();
			
			middle=middle.substring(1,middle.length()-2);
			
			if(middle.contains("=>")){
				//there is nested solution out there
				
				String tmpLeft="", tmpRight="";
				tmpLeft=middle.substring(0,middle.indexOf("=>"));
				tmpRight=middle.substring(middle.indexOf("=>")+2);
				//LEFT MIDDLE SIDE
				/**there is more then 1 solution**/
				if(tmpLeft.contains("+")){
					StringTokenizer st=new StringTokenizer(tmpLeft,"+");
					while(st.hasMoreTokens()){
						String sl=st.nextToken();
						if(sl.startsWith("(") && sl.endsWith(")")){
							lS.add(sl.substring(1,sl.length()-1));	
						}else{
							lS.add(sl);
						}	
						sl=null;
					}
					st=null;
				}
				/**there is only 1 solution**/
				else{
					if(tmpLeft.startsWith("(") && tmpLeft.endsWith(")")){
						tmpLeft=tmpLeft.substring(1,tmpLeft.lastIndexOf(")"));				
					}
					lS.add(tmpLeft);
				}
				//RIGHT MIDDLE SIDE
				/**there is more then 1 solution**/
				if(tmpRight.contains("+")){
					StringTokenizer st=new StringTokenizer(tmpRight,"+");
					while(st.hasMoreTokens()){
						String sl=st.nextToken();
						if(sl.startsWith("(") && sl.endsWith(")")){
							rS.add(sl.substring(1,sl.length()-1));	
						}else{
							rS.add(sl);
						}	
						sl=null;
					}
					st=null;
				}
				/**there is only 1 solution**/
				else{
					if(tmpRight.startsWith("(") && tmpRight.endsWith(")")){
						tmpRight=tmpRight.substring(1,tmpRight.lastIndexOf(")"));				
					}
					rS.add(tmpRight);
				}
				
			}else{
				//there is a modulator inside there			
				if(middle.startsWith("(") && middle.endsWith(")")){
					middle=middle.substring(1,middle.lastIndexOf(")"));
				}
				lS.add(middle);
				rS.add(middle);
			}
			for(int i=0;i<lS.size();i++){
				leftSolutions.add(lS.get(i).trim());
			}
			for(int i=0;i<rS.size();i++){
				rightSolutions.add(rS.get(i).trim());
			}
			
		}	
		
		draw(oRule);
	}
	
	
	private void draw(String rule){
		
		ArrayList<DefaultGraphCell> reactants=new ArrayList<DefaultGraphCell>();
		ArrayList<DefaultGraphCell> products=new ArrayList<DefaultGraphCell>();
		ArrayList<DefaultGraphCell> modulators=new ArrayList<DefaultGraphCell>();
		
		//Parser_SBGNRule2BiochamRule draw=new Parser_SBGNRule2BiochamRule(model.getReactionsGraphEditor());
		ArrayList<String> modSolutions=new ArrayList<String>();
		
		boolean self_loop=false;
		if(leftSolutions.size()==rightSolutions.size() && leftSolutions.size()==1){
			if(rightSolutions.get(0).equals(leftSolutions.get(0))){
				//its a self-loop
				self_loop=true;				
			}
		}
		if(!self_loop){
			for(int i=0;i<leftSolutions.size();i++){
				if(rightSolutions.contains(leftSolutions.get(i)) && !leftSolutions.get(i).trim().equals("_")){				
					modSolutions.add(leftSolutions.get(i));
				}
			}
			for(int i=0;i<modSolutions.size();i++){				
				leftSolutions.remove(modSolutions.get(i));
				rightSolutions.remove(modSolutions.get(i));			
			}
		}
		
		
		/***....CREATE OBJECTS FOR REACTANTS......**/
		if((leftSolutions.size()==1 && leftSolutions.get(0).trim().equals("_")) || leftSolutions.size()==0){
			BiochamEntityData data=new BiochamEntityData(model.getReactionsGraphEditor());
			data.setName("Source/Sink");
			ESourceSink cc=new ESourceSink(data);
			model.getReactionsGraphEditor().getGraphLayoutCache().insert(cc);
			reactants.add(cc);			
		}else{		
			//create reactants objects....	
			int s=leftSolutions.size();
			createSBGNfromBiocham_Objects(leftSolutions,reactants);
		}
		
		/***....CREATE OBJECTS FOR PRODUCTS......**/
		if((rightSolutions.size()==1 && rightSolutions.get(0).trim().equals("_")) || rightSolutions.size()==0){
			BiochamEntityData data=new BiochamEntityData(model.getReactionsGraphEditor());
			data.setName("Source/Sink");
			ESourceSink cc=new ESourceSink(data);
			model.getReactionsGraphEditor().getGraphLayoutCache().insert(cc);
			products.add(cc);
		}else{
			//the product is a multimer of the reactant.....
			if(reactants.size()==1 && ((BiochamEntityData)reactants.get(0).getUserObject()).getStoichoimetryIntegerForReaction()>1 && rightSolutions.get(0).contains("-")){
				ArrayList<String> l=new ArrayList<String>();
				StringTokenizer st=new StringTokenizer(rightSolutions.get(0),"-");
				while(st.hasMoreTokens()){
					l.add(st.nextToken());
				}
				String nmm=((BiochamEntityData)reactants.get(0).getUserObject()).getRepresentingName();
				int siz=l.size();
				int cnt=0;
				for(int k=0;k<l.size();k++){
					if(l.contains(nmm)){
						cnt++;
					}
				}
				if(l.size()==cnt){
					BiochamEntityData containingDt=new BiochamEntityData(model.getReactionsGraphEditor());
					containingDt.setMultimerCardinality(((BiochamEntityData)reactants.get(0).getUserObject()).getStoichoimetryIntegerForReaction());
					containingDt.setRepresentingName(nmm);
					containingDt.setName(rightSolutions.get(0));
					DefaultGraphCell cl0=GraphUtilities.getCellByName(model.getReactionsGraphEditor(), ((BiochamEntityData)reactants.get(0).getUserObject()).getName());
					DefaultGraphCell cell;
					if(cl0!=null){
						if(cl0 instanceof EComplexCell){
							containingDt.setContainingMolecules(((BiochamEntityData)cl0.getUserObject()).getContainingMolecules());
							cell=new EComplexCell(containingDt);
							model.getReactionsGraphEditor().getGraphLayoutCache().insert(cell);										
						}else if(cl0 instanceof ENucleicAcidFeatureCell){
							cell=new ENucleicAcidFeatureCell(containingDt);
							model.getReactionsGraphEditor().getGraphLayoutCache().insert(cell);
						}else{
							cell=new EMacromoleculeCell(containingDt);
							model.getReactionsGraphEditor().getGraphLayoutCache().insert(cell);
						}
					}else{
						cell=new EMacromoleculeCell(containingDt);
						model.getReactionsGraphEditor().getGraphLayoutCache().insert(cell);
					}
					products.add(cell);
				}else{
					findOutProductElements(products);
				}
				st=null;
				l.clear();
				l=null;
				nmm=null;
				
			}else{
				//the product is a complex primary of reactants perhaps....
				if(rightSolutions.size()==1 && rightSolutions.get(0).contains("-")){
					findOutProductElements(products);
				}else{				
					createSBGNfromBiocham_Objects(rightSolutions,products);
				}
				
				
			}
			
		}
		
		
		
		/***....CREATE OBJECTS FOR MODULATORS......**/
		for(int i=0;i<modSolutions.size();i++){
			//create modulators objects....	
			createSBGNfromBiocham_Objects(modSolutions,modulators);
		}
		
		
		
		BiochamEdgeData eData=new BiochamEdgeData(model.getReactionsGraphEditor());
		eData.setKinetics(kinetics);
		eData.setName(reactionName);
		eData.setReversible(reversible);		
		GraphUtilities.setReactantsFromCells(eData,reactants);	
		int siz=products.size();
		for(int y=0;y<siz;y++){
			Utils.debugMsg(((BiochamEntityData)products.get(y).getUserObject()).getName());
		}
		GraphUtilities.setProductsFromCells(eData,products);
		siz=products.size();
		for(int y=0;y<siz;y++){
			Utils.debugMsg(((BiochamEntityData)products.get(y).getUserObject()).getName());
		}
		
		GraphUtilities.setModulators1FromCells(eData,modulators);
		if(reversible){
			GraphUtilities.setModulators2FromCells(eData,modulators);
		}
	
		ArrayList<UUID> molecules=new ArrayList<UUID>();
		
		for(int i=0;i<reactants.size();i++){
			molecules.add(((BiochamEntityData)reactants.get(i).getUserObject()).getId());
		}
		for(int i=0;i<products.size();i++){
			molecules.add(((BiochamEntityData)products.get(i).getUserObject()).getId());
		} 
		if(modulators.size()>0){
			for(int i=0;i<modulators.size();i++){
				molecules.add(((BiochamEntityData)modulators.get(i).getUserObject()).getId());
			}
		}		
  	  	eData.setMolecules(molecules);  	  		  
		DefaultGraphCell st;			
		
		
		/***....DEFINE WHICH TYPE OF REACTION IT IS......**/
		
		
		/***....CREATE, AND INSERT THE REACTION.......................**/
		
		//multimerization
		if(reactants.size()==1 && !(reactants.get(0) instanceof ESourceSink) && ((BiochamEntityData)reactants.get(0).getUserObject()).getStoichoimetryIntegerForReaction()>1 && products.size()==1 && !(products.get(0) instanceof ESourceSink) && ((BiochamEntityData)products.get(0).getUserObject()).getMultimerCardinality()>1){
			eData.setType("Association");  
			if(reversible){
				st=new ReversibleAssociation(eData,reactants,products.get(0),null,model.getReactionsGraphEditor(),rule);
				((ReversibleAssociation)st).setReversibleDissociation(false);
				if(modulators!=null){
					if(modulators.size()>0){
						for(int i=0;i<modulators.size();i++){
							((ReversibleAssociation)st).addModulator1(((BiochamEntityData)modulators.get(i).getUserObject()).getName(),1, false);
							((ReversibleAssociation)st).addModulator2(((BiochamEntityData)modulators.get(i).getUserObject()).getName(),1, false);
						}
					}
				}
				reversible=false;
			}else{
				st=new Association(eData,reactants,products.get(0),null,model.getReactionsGraphEditor(),rule);
				if(modulators!=null){
					if(modulators.size()>0){
						for(int i=0;i<modulators.size();i++){
							((Association)st).addModulator(((BiochamEntityData)modulators.get(i).getUserObject()).getName(),1, false);
						}
					}
				}
			}
		}
		//de-multimerization
		else if(products.size()==1 && !(products.get(0) instanceof ESourceSink) && ((BiochamEntityData)products.get(0).getUserObject()).getStoichoimetryIntegerForReaction()>1 && reactants.size()==1 && !(reactants.get(0) instanceof ESourceSink) && ((BiochamEntityData)reactants.get(0).getUserObject()).getMultimerCardinality()>1){
			eData.setType("Dissociation");
			if(reversible){
				st=new ReversibleAssociation(eData,products,reactants.get(0),null,model.getReactionsGraphEditor(),rule);
				((ReversibleAssociation)st).setReversibleDissociation(true);
				if(modulators!=null){
					if(modulators.size()>0){
						for(int i=0;i<modulators.size();i++){
							((ReversibleAssociation)st).addModulator1(((BiochamEntityData)modulators.get(i).getUserObject()).getName(),1, false);
							((ReversibleAssociation)st).addModulator2(((BiochamEntityData)modulators.get(i).getUserObject()).getName(),1, false);
						}
					}
				}
				reversible=false;
			}else{
				st=new Dissociation(eData,reactants.get(0),products,null,model.getReactionsGraphEditor(),rule);
				if(modulators!=null){
					if(modulators.size()>0){
						for(int i=0;i<modulators.size();i++){
							((Dissociation)st).addModulator(((BiochamEntityData)modulators.get(i).getUserObject()).getName(),1, false);
						}
					}
				}
			}
		}
		//association
		else if(reactants.size()>1 && products.size()==1 && products.get(0) instanceof EComplexCell){
			eData.setType("Association");  
			if(reversible){
				st=new ReversibleAssociation(eData,reactants,products.get(0),null,model.getReactionsGraphEditor(),rule);	
				((ReversibleAssociation)st).setReversibleDissociation(false);
				if(modulators!=null){
					if(modulators.size()>0){
						for(int i=0;i<modulators.size();i++){
							((ReversibleAssociation)st).addModulator1(((BiochamEntityData)modulators.get(i).getUserObject()).getName(),1, false);
							((ReversibleAssociation)st).addModulator2(((BiochamEntityData)modulators.get(i).getUserObject()).getName(),1, false);
						}
					}
				}
				reversible=false;
			}else{
				st=new Association(eData,reactants,products.get(0),null,model.getReactionsGraphEditor(),rule);
				if(modulators!=null){
					if(modulators.size()>0){
						for(int i=0;i<modulators.size();i++){
							((Association)st).addModulator(((BiochamEntityData)modulators.get(i).getUserObject()).getName(),1, false);
						}
					}
				}
			}
		}
		//dissociation
		else if(products.size()>1 && reactants.size()==1 && reactants.get(0) instanceof EComplexCell){
			eData.setType("Dissociation");
			if(reversible){
				st=new ReversibleAssociation(eData,products,reactants.get(0),null,model.getReactionsGraphEditor(),rule);
				((ReversibleAssociation)st).setReversibleDissociation(true);
				if(modulators!=null){
					if(modulators.size()>0){
						for(int i=0;i<modulators.size();i++){
							((ReversibleAssociation)st).addModulator1(((BiochamEntityData)modulators.get(i).getUserObject()).getName(),1, false);
							((ReversibleAssociation)st).addModulator2(((BiochamEntityData)modulators.get(i).getUserObject()).getName(),1, false);
						}
					}
				}
				reversible=false;
			}else{
				st=new Dissociation(eData,reactants.get(0),products,null,model.getReactionsGraphEditor(),rule);
				if(modulators!=null){
					if(modulators.size()>0){
						for(int i=0;i<modulators.size();i++){
							((Dissociation)st).addModulator(((BiochamEntityData)modulators.get(i).getUserObject()).getName(),1, false);
						}
					}
				}
			}
		}
		//state transition
		else{
			eData.setType("StateTransition");
			if(reversible){
				st=new StateTransition(eData,reactants,products,null,null,model.getReactionsGraphEditor(),true,rule);
				if(modulators!=null){
					if(modulators.size()>0){
						for(int i=0;i<modulators.size();i++){
							((StateTransition)st).addModulator(((BiochamEntityData)modulators.get(i).getUserObject()).getName(),1, false);
						}
					}
				}
				reversible=false;
			}else{
				st=new StateTransition(eData,reactants,products,null,null,model.getReactionsGraphEditor(),false,rule);
				if(modulators!=null){
					if(modulators.size()>0){
						for(int i=0;i<modulators.size();i++){
							((StateTransition)st).addModulator(((BiochamEntityData)modulators.get(i).getUserObject()).getName(),1, false);
						}
					}
				}
			}
		}
				
		
	
		
	}


	/**
	 * @param products
	 */
	private void findOutProductElements(ArrayList<DefaultGraphCell> products) {
		BiochamEntityData data=new BiochamEntityData(model.getReactionsGraphEditor());
		String stoich = null;
		String reprName="";
		String nm=rightSolutions.get(0).trim();
		if(nm.contains("*")){
			stoich=nm.substring(0,nm.indexOf("*"));
			nm=nm.substring(nm.indexOf("*")+1);				
			data.setStoichoimetryForReaction(stoich);
		}
		if(nm.startsWith("(") && nm.endsWith(")")){
			nm=nm.substring(1,nm.lastIndexOf(")"));
		}
		data.setName(nm);
		if(nm.contains("::")){
			data.setCompartment(nm.substring(nm.lastIndexOf("::")+2));
			nm=nm.substring(0,nm.lastIndexOf("::"));
		}
		if(nm.contains(")~")){
			String modifSites=nm.substring(nm.lastIndexOf("{")+1,nm.lastIndexOf("}"));
			nm=nm.substring(0,nm.lastIndexOf("~")).trim();
			data.setMoleculeState(MoleculeState.MODIFIED);
			data.setModificationSites(modifSites);
		}	
		if(nm.startsWith("(") && nm.endsWith(")")){
			nm=nm.substring(1,nm.lastIndexOf(")"));
		}
							
		DefaultGraphCell c=GraphUtilities.getCellByName(model.getReactionsGraphEditor(), nm);
		if(c!=null){
			//it exists already this element....Great!!!!Use it......						
			if(stoich!=null){						
				((BiochamEntityData)c.getUserObject()).setStoichoimetryForReaction(stoich);
			}
			products.add(c);		
		}else{
			ArrayList<DefaultGraphCell> containingMolecules=new ArrayList<DefaultGraphCell>();
			boolean turn=true;
			String name=nm;
			String nm2=nm;
			boolean justFinish=false;
			while(name.length()>0){						
				while(turn){
				
					c=GraphUtilities.getCellByName(model.getReactionsGraphEditor(), name);
					if(c!=null){
						turn=false;
					}else{
						if(name.contains("-")){
							String s1=name.substring(0,name.lastIndexOf("-"));
							c=GraphUtilities.getCellByName(model.getReactionsGraphEditor(), s1);
							if(c!=null){
								turn=false;
							}else{
								if(s1.contains("-")){
									name=s1;	
								}else{
								
									name=s1;
									/*turn=false;
									justFinish=true;*/
									BiochamEntityData dt=new BiochamEntityData(model.getReactionsGraphEditor());											
										
									dt.setName(s1);
									if(s1.contains("::")){									
										dt.setCompartment(s1.substring(s1.lastIndexOf("::")+2));
										s1=s1.substring(0,s1.lastIndexOf("::"));
									}
									if(s1.contains("~")){																								
										dt.setMoleculeState(MoleculeState.MODIFIED);
										dt.setModificationSites(s1.substring(s1.indexOf("{")+1,s1.lastIndexOf("}")));
										s1=s1.substring(0,s1.indexOf("~"));
									}	
									dt.setRepresentingName(s1);
									if(s1.startsWith("#")){
										dt.setMoleculeType(MoleculeType.NUCLEIC_ACID_FEATURE);
										c=new ENucleicAcidFeatureCell(dt);												
									}else{
										dt.setMoleculeType(MoleculeType.MACROMOLECULE);
										c=new EMacromoleculeCell(dt);
									}
									//model.getReactionsGraphEditor().getGraphLayoutCache().insert(c);
									containingMolecules.add(c);	
									if(!reprName.equals("")){
										reprName+="-";
									}
									reprName+=s1;
									s1=nm.substring(nm.indexOf(dt.getName())+dt.getName().length()+1).trim();
									name=s1;
								}
									/*c=GraphUtilities.getCellByName(model.getReactionsGraphEditor(), s1);
									if(c==null){
										dt=new BiochamEntityData(model.getReactionsGraphEditor());											
										
										dt.setName(s1);								
										if(s1.contains("::")){									
											dt.setCompartment(s1.substring(s1.lastIndexOf("::")+2));
											s1=s1.substring(0,s1.lastIndexOf("::"));
										}
										
										if(s1.contains("~")){																								
											dt.setMoleculeState(MoleculeState.MODIFIED);
											dt.setModificationSites(s1.substring(s1.indexOf("{")+1,s1.lastIndexOf("}")));
											s1=s1.substring(0,s1.indexOf("~"));
										}	
										dt.setRepresentingName(s1);		
										if(s1.startsWith("#")){
											dt.setMoleculeType(MoleculeType.NUCLEIC_ACID_FEATURE);
											c=new ENucleicAcidFeatureCell(dt);												
										}else{
											dt.setMoleculeType(MoleculeType.MACROMOLECULE);
											c=new EMacromoleculeCell(dt);
										}
										//model.getReactionsGraphEditor().getGraphLayoutCache().insert(c);
									}
									reprName+="-"+s1;
									containingMolecules.add(c);	*/
								//}
							}
						}else{
							turn=false;
							justFinish=true;
							BiochamEntityData dt=new BiochamEntityData(model.getReactionsGraphEditor());											
								
							dt.setName(name);
							if(name.contains("::")){									
								dt.setCompartment(name.substring(name.lastIndexOf("::")+2));
								name=name.substring(0,name.lastIndexOf("::"));
							}
							if(name.contains("~")){																								
								dt.setMoleculeState(MoleculeState.MODIFIED);
								dt.setModificationSites(name.substring(name.indexOf("{")+1,name.lastIndexOf("}")));
								name=name.substring(0,name.indexOf("~"));
							}	
							dt.setRepresentingName(name);
							if(name.startsWith("#")){
								dt.setMoleculeType(MoleculeType.NUCLEIC_ACID_FEATURE);
								c=new ENucleicAcidFeatureCell(dt);												
							}else{
								dt.setMoleculeType(MoleculeType.MACROMOLECULE);
								c=new EMacromoleculeCell(dt);
							}
							//model.getReactionsGraphEditor().getGraphLayoutCache().insert(c);
							containingMolecules.add(c);	
							if(!reprName.equals("")){
								reprName+="-";
							}
							reprName+=name;
						}
															
					}								
				}
				if(!justFinish){
					if(!reprName.equals(((BiochamEntityData)c.getUserObject()).getRepresentingName())){
						if(!reprName.equals("")){
							reprName+="-";
						}
						reprName+=((BiochamEntityData)c.getUserObject()).getRepresentingName();
					}
					containingMolecules.add(c);	
					if(!(nm.endsWith(((BiochamEntityData)c.getUserObject()).getName()) && nm2.equals(((BiochamEntityData)c.getUserObject()).getName()))){
						nm2=nm2.substring(nm2.indexOf(((BiochamEntityData)c.getUserObject()).getName())+((BiochamEntityData)c.getUserObject()).getName().length()+1).trim();
						name=nm2;//nm2.substring(nm2.indexOf(((BiochamEntityData)c.getUserObject()).getName())+((BiochamEntityData)c.getUserObject()).getName().length()+1).trim();
						turn=true;	
					}else{
						turn=false;
						break;
					}	
				}else{
					break;
				}
										
			}
			if(containingMolecules.size()>0){
				data.setMoleculeType(MoleculeType.COMPLEX);						
				ArrayList<ContainingMolecule> containingMols=new ArrayList<ContainingMolecule>(containingMolecules.size());
				int card=1;
				ArrayList<DefaultGraphCell> molsAdd=new ArrayList<DefaultGraphCell>();
				for(int y=0;y<containingMolecules.size();y++){
					if(y>0 && molsAdd.contains(containingMolecules.get(y))){
						card++;
						containingMols.get(containingMols.size()-1).setCardinality(card);
					}else{
						card=1;									
						BiochamEntityData dt=((BiochamEntityData)containingMolecules.get(y).getUserObject());
						containingMols.add(new ContainingMolecule(dt.getName(),dt.getMoleculeState(),dt.getMultimerCardinality(),dt.isModulator(),dt.getMoleculeType(),dt.getInitialConcentration(),dt.getRepresentingName(),dt.getId(),containingMolecules.get(y)));
						molsAdd.add(containingMolecules.get(y));
					}
					
				}
				molsAdd.clear();molsAdd=null;
				if(card>1){
					data.setContainingMolecules(((BiochamEntityData)containingMols.get(0).getUserObject()).getContainingMolecules());
				}else{
					data.setContainingMolecules(containingMols);	
				}
					
				
				data.setRepresentingName(reprName);
				data.setMultimerCardinality(card);
				EComplexCell cc=new EComplexCell(data);
				products.add(cc);
				model.getReactionsGraphEditor().getGraphLayoutCache().insert(cc);
			}
		}
	}


	/**
	 * @param reactants
	 */
	private void createSBGNfromBiocham_Objects(ArrayList<String> leftSolutions, ArrayList<DefaultGraphCell> reactants) {
		for(int i=0;i<leftSolutions.size();i++){
			//create reactants objects....
			BiochamEntityData data=new BiochamEntityData(model.getReactionsGraphEditor());
			String nm=leftSolutions.get(i).trim();
			String stoich = null,compartment,modifSites;
			
			if(nm.contains("*")){
				stoich=nm.substring(0,nm.indexOf("*"));
				nm=nm.substring(nm.indexOf("*")+1).trim();				
				data.setStoichoimetryForReaction(stoich);
			}
			if(nm.startsWith("(") && nm.endsWith(")")){
				nm=nm.substring(1,nm.lastIndexOf(")")).trim();
			}
			
			DefaultGraphCell c=GraphUtilities.getCellByName(model.getReactionsGraphEditor(), nm);
			if(c!=null){
				//it exists already this element....Great!!!!Use it......
				if(stoich!=null){						
					((BiochamEntityData)c.getUserObject()).setStoichoimetryForReaction(stoich);
				}
				reactants.add(c);
				nm=null;
				data=null;					
			}else{
				//it doesn't exist yet, create it....
				data.setName(nm);
				/***...If it's not a Complex......**/
				if(!nm.contains("-")){
					if(nm.contains("::")){
						String comp=nm.substring(nm.lastIndexOf("::")+2);
						nm=nm.substring(0,nm.lastIndexOf("::")).trim();
						data.setCompartment(comp);	
					}
					data.setName(nm);
					if(nm.contains("~")){
						modifSites=nm.substring(nm.indexOf("{")+1,nm.lastIndexOf("}"));
						nm=nm.substring(0,nm.indexOf("~")).trim();
						data.setMoleculeState(MoleculeState.MODIFIED);
						data.setModificationSites(modifSites);
					}					
					if(nm.startsWith("#")){
						data.setMoleculeType(MoleculeType.NUCLEIC_ACID_FEATURE);
						data.setRepresentingName(nm);
						ENucleicAcidFeatureCell cc=new ENucleicAcidFeatureCell(data);
						reactants.add(cc);
						model.getReactionsGraphEditor().getGraphLayoutCache().insert(cc);		
					}else{
						data.setMoleculeType(MoleculeType.MACROMOLECULE);						
						data.setRepresentingName(nm);
						String nnmm=data.getName();
						EMacromoleculeCell cc=new EMacromoleculeCell(data);
						reactants.add(cc);
						model.getReactionsGraphEditor().getGraphLayoutCache().insert(cc);				
					}
				}
				/***...If it's a Complex......**/
				else{
					String mol="";
					String tmp=nm.substring(nm.lastIndexOf("-")+2);
					if(tmp.contains("::")){
						String comp=nm.substring(nm.lastIndexOf("::")+2);
						nm=nm.substring(0,nm.lastIndexOf("::")).trim();
						data.setCompartment(comp);
						comp=null;
					}	
					tmp=null;
					if(nm.contains(")~")){
						modifSites=nm.substring(nm.lastIndexOf("{")+1,nm.lastIndexOf("}"));
						nm=nm.substring(0,nm.lastIndexOf("~")).trim();
						data.setMoleculeState(MoleculeState.MODIFIED);
						data.setModificationSites(modifSites);
					}					
					//it can be a multimer of: SimpleMolecule or Complex, or it can be just a Complex						
					ArrayList<DefaultGraphCell> containingMolecules=new ArrayList<DefaultGraphCell>();
					StringTokenizer st=new StringTokenizer(nm,"-");
					int cnt=st.countTokens();
					if(cnt>0){
						/**There are at least 2 molecules....*/
						mol=st.nextToken().trim();
						while(st.hasMoreTokens()){
							int mol_i=1;
							String nextMol = null;
							while(st.hasMoreTokens()){
								nextMol=st.nextToken().trim();
								if(mol.equals(nextMol)){
									mol_i++;
								}else{
									break;
								}
							}
							/**The containing molecule its a multimer....*/
							if(mol_i>1){
								DefaultGraphCell cl=GraphUtilities.getCellByName(model.getReactionsGraphEditor(), nm.substring(0,nm.lastIndexOf(mol)-1));
								if(cl!=null){
									containingMolecules.add(cl);
								}else{
									//create a multimer...NOT A COMPLEXXXXX!!!!!!!!
									BiochamEntityData containingDt=new BiochamEntityData(model.getReactionsGraphEditor());
									containingDt.setMultimerCardinality(mol_i);
									containingDt.setRepresentingName(mol);
									containingDt.setName(nm.substring(0,nm.lastIndexOf(mol)+mol.length()).trim());
									DefaultGraphCell cl0=GraphUtilities.getCellByName(model.getReactionsGraphEditor(), mol);
									if(cl0!=null){
										if(cl0 instanceof EComplexCell){
											containingDt.setContainingMolecules(((BiochamEntityData)cl0.getUserObject()).getContainingMolecules());
											EComplexCell cell=new EComplexCell(containingDt);
											containingMolecules.add(cell);											
										}else if(cl0 instanceof ENucleicAcidFeatureCell){
											ENucleicAcidFeatureCell cell=new ENucleicAcidFeatureCell(containingDt);
											containingMolecules.add(cell);
										}else{
											EMacromoleculeCell cell=new EMacromoleculeCell(containingDt);
											containingMolecules.add(cell);
										}
									}else{
										containingDt.setCompartment(data.getCompartment());
										containingDt.setMoleculeState(data.getMoleculeState());
										containingDt.setModificationSites(data.getModificationSites());
										if(mol.startsWith("#")){
											ENucleicAcidFeatureCell cell=new ENucleicAcidFeatureCell(containingDt);
											containingMolecules.add(cell);
										}else{
											EMacromoleculeCell cell=new EMacromoleculeCell(containingDt);
											containingMolecules.add(cell);	
										}
										
									}
								}
							}
							/**The containing molecule it's a simple molecule......*/
							else{
								
								DefaultGraphCell cl=GraphUtilities.getCellByName(model.getReactionsGraphEditor(), mol);
								if(cl!=null){
									containingMolecules.add(cl);
								}else{
									BiochamEntityData containingDt=new BiochamEntityData(model.getReactionsGraphEditor());							
									containingDt.setName(mol);	
									if(mol.contains("~")){										
										containingDt.setMoleculeState(MoleculeState.MODIFIED);
										containingDt.setModificationSites(mol.substring(mol.lastIndexOf("{")+1,mol.lastIndexOf("}")));
										mol=mol.substring(0,mol.lastIndexOf("~")).trim();
									}	
									containingDt.setRepresentingName(mol);
									//containingDt.setMoleculeState(data.getMoleculeState());
									//containingDt.setModificationSites(data.getModificationSites());
									containingDt.setCompartment(data.getCompartment());
									if(mol.startsWith("#")){
										containingDt.setMoleculeType(MoleculeType.NUCLEIC_ACID_FEATURE);
										containingMolecules.add(new ENucleicAcidFeatureCell(containingDt));
									}else{
										containingDt.setMoleculeType(MoleculeType.MACROMOLECULE);
										containingMolecules.add(new EMacromoleculeCell(containingDt));
									}
								}
							}
							if(mol.equals(nextMol)){
								mol="";
							}else{
								mol=nextMol;
							}
							
						}
						if(mol.contains("::")){
							compartment=mol.substring(mol.indexOf("::"));
							mol=mol.substring(0,mol.indexOf("::")).trim();
							data.setCompartment(compartment);
						}
						if(mol!=""){
							DefaultGraphCell cl=GraphUtilities.getCellByName(model.getReactionsGraphEditor(), mol);
							if(cl!=null){
								containingMolecules.add(cl);
							}else{
								BiochamEntityData containingDt=new BiochamEntityData(model.getReactionsGraphEditor());							
								containingDt.setName(mol);	
								if(mol.contains("~")){										
									containingDt.setMoleculeState(MoleculeState.MODIFIED);
									containingDt.setModificationSites(mol.substring(mol.lastIndexOf("{")+1,mol.lastIndexOf("}")));
									mol=mol.substring(0,mol.lastIndexOf("~")).trim();
								}	
								containingDt.setRepresentingName(mol);
								containingDt.setCompartment(data.getCompartment());
								if(mol.startsWith("#")){
									containingDt.setMoleculeType(MoleculeType.NUCLEIC_ACID_FEATURE);
									containingMolecules.add(new ENucleicAcidFeatureCell(containingDt));
								}else{
									containingDt.setMoleculeType(MoleculeType.MACROMOLECULE);
									containingMolecules.add(new EMacromoleculeCell(containingDt));
								}
							}
						}
					}
					if(containingMolecules.size()>1){						
					
						// CREATE THE COMPLEX NOW //
						data.setMoleculeType(MoleculeType.COMPLEX);
						data.setRepresentingName(nm);
						ArrayList<ContainingMolecule> containingMols=new ArrayList<ContainingMolecule>(containingMolecules.size());
						for(int y=0;y<containingMolecules.size();y++){
							BiochamEntityData dt=((BiochamEntityData)containingMolecules.get(y).getUserObject());
							containingMols.add(new ContainingMolecule(dt.getName(),dt.getMoleculeState(),dt.getMultimerCardinality(),dt.isModulator(),dt.getMoleculeType(),dt.getInitialConcentration(),dt.getRepresentingName(),dt.getId(),containingMolecules.get(y)));
						}
						data.setContainingMolecules(containingMols);
					
						EComplexCell cc=new EComplexCell(data);
						reactants.add(cc);
						model.getReactionsGraphEditor().getGraphLayoutCache().insert(cc);
					}else{
						BiochamEntityData dt=((BiochamEntityData)containingMolecules.get(0).getUserObject());
						dt.setCompartment(data.getCompartment());
						dt.setMoleculeState(data.getMoleculeState());
						dt.setModificationSites(data.getModificationSites());
						containingMolecules.get(0).setUserObject(dt);
						reactants.add(containingMolecules.get(0));
						model.getReactionsGraphEditor().getGraphLayoutCache().insert(containingMolecules.get(0));
					}
				}	
			}					
		}
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	public String getKinetics() {
		return kinetics;
	}


	public void setKinetics(String kinetics) {
		this.kinetics = kinetics;
	}


	public String getModulators() {
		return modulators;
	}


	public void setModulators(String modulators) {
		this.modulators = modulators;
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

}
