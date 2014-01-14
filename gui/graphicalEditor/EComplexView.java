package fr.inria.contraintes.biocham.graphicalEditor;

import org.jgraph.graph.CellViewRenderer;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.VertexRenderer;
import org.jgraph.graph.VertexView;

import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.font.FontRenderContext;
import java.awt.geom.AffineTransform;
import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.StringTokenizer;
import java.util.UUID;
import javax.swing.BorderFactory;


public class EComplexView extends VertexView{

	
	public static int counterOfPaintCalls=0;
	
	public transient ComplexRenderer renderer=new ComplexRenderer();
	protected BiochamEntityData userObject;
	ArrayList<ContainingMolecule> containingMolecules;
	double maxWidth, maxHeight;
	Dimension containingMolDimension=new Dimension(EMacromoleculeCell.WIDTH,EMacromoleculeCell.HEIGHT-15);
	int containingMolArc =12;		
	static BasicStroke bs1=new BasicStroke(1);
	
	public EComplexView(Object cell) {		
		super(cell);
		complexViewUI(cell);
		//userObject.getGraph().refresh();
	}
		

	private void complexViewUI(Object cell) {		
		
		if(cell instanceof EComplexCell){
				
			double x,y;
			x=0;
			y=0;
			userObject=((BiochamEntityData)((EComplexCell)cell).getUserObject());
			
			if(userObject.getPosition()!=null){
			
				if(!(userObject.getPosition().getX()>0 && userObject.getPosition().getY()>0)){
					bounds=BiochamGraphConstants.getBounds(((DefaultGraphCell)cell).getAttributes());
					if(bounds!=null){
						x=bounds.getX();
						y=bounds.getY();		
					}		
					userObject.setPosition(new Position(x,y,0,0));					
				}
				
			}else{
				
				bounds=BiochamGraphConstants.getBounds(((DefaultGraphCell)cell).getAttributes());
				if(bounds!=null){
					x=bounds.getX();
					y=bounds.getY();		
				}		
				userObject.setPosition(new Position(x,y,0,0));	
			}
			
			
			containingMolecules=userObject.getContainingMolecules();			
			
			if(containingMolecules!=null){
				maxHeight=(containingMolecules.size())*EMacromoleculeCell.HEIGHT;
			}else{
				maxHeight=EMacromoleculeCell.HEIGHT;
			}			
			
			maxWidth=EMacromoleculeCell.WIDTH;//*1.5;		
			
			double w=EMacromoleculeCell.WIDTH;
			double h=EMacromoleculeCell.HEIGHT;
			
			if(userObject.getSize()==null){
				if(bounds!=null){
					w=bounds.getWidth();
					h=bounds.getHeight();
				}
				userObject.setSize(new MoleculeSize(w,h));
			}
			
			userObject.setBounds(bounds);
			
			
		}
	}
		
	
	
	/**************************   COMPLEX RENDERER   ***********************************/
	public class ComplexRenderer extends VertexRenderer {
		
		
		
		
		
		/**************************   PAINT METHOD   ***********************************/
		public void paint(Graphics g) {	
			
			
			Utils.debugMsg("BEFORE: "+userObject.getPosition().getX()+","+userObject.getPosition().getY());
			setDoubleBuffered(true);
			setOpaque(false);
			
			Graphics2D g2 = (Graphics2D) g;
			g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON);
			g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,RenderingHints.VALUE_TEXT_ANTIALIAS_ON);				
			g.setColor(super.getBackground());							
			setBorder(BorderFactory.createRaisedBevelBorder());
			paintComplex(g, g2,0,0, borderWidth, getSize(), selected, userObject);
			g2=null;			
		}
		
		
		
		/**************************  SUB-PAINT METHOD   ***********************************/
		private void paintComplex(Graphics g, Graphics2D g2,int startX, int startY, int b, Dimension d, boolean tmp, BiochamEntityData userObject) {
			
			/**coordinates initialization**/
			
			int h=d.height;
			int w=d.width;	

			
			//userObject.setSize(new MoleculeSize(w,h));
			
			
			int x=20, y=15, z=15;
			int maxLen=0;
			int complexWidth=0;			
			if(w<80){
				x=10;
				y=10;
				z=5;				
			}
			
			//userObject.setCompact(true);
			if(userObject.isCompact()){
				if(!userObject.isSetAsCompact()){
					h=h/3;
					d.height=h/3;
					userObject.getSize().height=h;		
					userObject.applySize();
					userObject.setSetAsCompact(true);
				}						
			}else{
				if(userObject.isSetAsCompact()){
					h=h*3;
					d.height=h*3;
					userObject.getSize().height=h;		
					userObject.applySize();
					userObject.setSetAsCompact(false);
				}
			}
			
			if(userObject!=null){
				
				/** (1)color initialization**/	
				if(userObject.getColor()==null){
					if(userObject.isModulator()){
						gradientColor=Color.RED.darker();
					}else{				
						gradientColor=Color.PINK;	
					}
				}else{
					gradientColor=userObject.getColor();
				}
				GradientPaint pntColor=new GradientPaint(0, 0, getBackground(),getWidth(), getHeight(), gradientColor, true);
				/** (2)representing name initialization**/				
				String complexName;
				if(userObject.getRepresentingName()==null){
					userObject.setRepresentingName(userObject.getName());
				}			
				/**See if its a multimer about the name+ position it in the middle, not down as usually...**/
				containingMolecules=userObject.getContainingMolecules();
				if(userObject.getRepresentingName().contains("-") && containingMolecules==null){				
					String tmpStr=userObject.getRepresentingName().substring(userObject.getRepresentingName().lastIndexOf("-")+1,userObject.getRepresentingName().length());	
					String name=userObject.getRepresentingName().substring(0,userObject.getRepresentingName().indexOf(tmpStr))+tmpStr;						
					complexName=name;					
					tmpStr=null;
					name=null;
				}else{			
					complexName=userObject.getRepresentingName();				
				}				
				setOpaque(false);			
				
				if (!preview) {
					
					
					
					/**					 1. 					**/
					/**....DRAW THE COMPLEX AS A WHOLE FISRT....**/
					/**					    					**/
					/**					    					**/
					/**					    					**/
					/**					    					**/									
					if(userObject.getMultimerCardinality()>1){					
						/**                                            **/
						/**                                            **/
						/**............COMPLEX IS A MULTIMER...........**/
						/**                                            **/
						/**                                            **/					  
						g.setClip(startX,startY,w+10,h+10);					
						if(userObject.isCopy()){
							/**............COPY OF COMPLEX AS A MULTIMER...........**/
							g2.setClip(startX, startY, w+40, h+80);
							x+=2;
							y-=8;							
							drawCopyOfComplexMultimer(g, g2, b, userObject, x, y, z, complexName, h, w, pntColor);
						}else{	
							/**............ORDINARY COMPLEX AS A MULTIMER...........**/
							drawComplexMultimer(g, g2, b, d, tmp, x, y, z, complexName, h, w, pntColor);					
						}	
						/**CARDINALITY**/
						drawComplexCardinality(g, g2, d, userObject);					
						/**CONCENTRATION**/
						drawComplexInitialConcentration(g, d, userObject);		
						try {
							setBorder(null);
							setOpaque(false);
							selected = false;
							super.paint(g);
						} finally {
							selected = tmp;
						}
						bordercolor=Color.black;;
						setBorder(BorderFactory.createRaisedBevelBorder());						
					
					}else{					
						/**                                            **/
						/**                                            **/
						/**...........COMPLEX IS NOT A MULTIMER........**/
						/**                                            **/
						/**                                            **/
						if(userObject.isCopy()){
							/**................COPY OF COMPLEX..................**/
							drawCopyOfAComplex(g, g2, b, userObject, complexName, h, w, pntColor);
						}else{
							/**.................DRAW COMPLEX..................**/
							complexWidth = drawComplex(g, g2, complexName, h, w, pntColor);
						}
					}			
					/**............COMPLEX'S MODIFICATION SITES...........**/					
					if(userObject.getMoleculeState()==MoleculeState.MODIFIED && userObject.getModificationSites()!=null){					
						maxLen = drawComplexModificationSites(g, g2, d, userObject, w);					
					}
				
					
					
					/**					      2. 					  **/
					/**....DRAW THE COMPLEX'S CONTAINING MOLECULES....**/				
					/**					    					      **/
					/**					    					      **/
					/**					    					      **/				
			
					Dimension origin=d;
					d=containingMolDimension;
					int x0,y0;
					x0=origin.width/15;
					if(userObject!=null){
						if(userObject.getContainingMolecules()!=null){
							if(userObject.getContainingMolecules().size()>3){
								x0=origin.width/5;
							}
						}
					}
					
					if(userObject.getMoleculeState()==MoleculeState.MODIFIED){
						y0=20;
					}else{
						y0=7;
					}
					
					/**.........IF userObject.isCompact==false.............**/
					if(!userObject.isCompact()){
						/**....Show all the containing elements....**/
						/**......of the nested complexes also......**/	
						
						
						/*					       3. 					       */
						//......DRAW THE COMPLEX'S CONTAINING MOLECULES.........
						/*					      					           */		
						
						
						if(containingMolecules!=null){		
							
							for(int i=0;i<containingMolecules.size();i++){													
							
								
								
								
								     int card=containingMolecules.get(i).getCardinality();	
								     String nmm=containingMolecules.get(i).getName();
								     int incrWidth=0;
								     if(((BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject()).getMoleculeState()==MoleculeState.MODIFIED){
								    	 String modifSites=((BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject()).getModificationSites();
								    	 if(modifSites!=null){
								    			FontRenderContext frc = new FontRenderContext(null, false, false);	
												int len=(int)g.getFont().getStringBounds(modifSites, frc).getWidth();
												if(len+8>d.width){
													incrWidth+=(len-d.width);
													d.width+=incrWidth;
												}
								    	 }
								     }
								    		
								     
									//System.out.println("card="+card+",name="+((BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject()).getMultimerCardinality()+",name="+containingMolecules.get(i).getName());						
									
									g.setColor(super.getBackground());
									DefaultGraphCell c=null;
									try{
										c=containingMolecules.get(i).getInstance();
									}catch(Exception e){	
										e.printStackTrace();
									}
									if(c==null){
										try{
											c=containingMolecules.get(i).getInstance();
											UUID id=UUID.randomUUID();
											BiochamEntityData ddt=((BiochamEntityData)c.getUserObject());
											ddt.setId(id);
											containingMolecules.get(i).setId(id);
											c.setUserObject(ddt);								
											id=null;
											ddt=null;								
										}catch(Exception ee){
										}
									}
									if(c==null){
										BiochamEntityData dt=new BiochamEntityData(userObject.getGraph());
										dt.setColor(containingMolecules.get(i).getColor());
										dt.setInitialConcentration(containingMolecules.get(i).getInitialConcentration());
										dt.setMultimerCardinality(containingMolecules.get(i).getCardinality());
										dt.setId(containingMolecules.get(i).getId());
										dt.setName(containingMolecules.get(i).getName());
										dt.setRepresentingName(containingMolecules.get(i).getRepresentingName());
										dt.setMoleculeState(containingMolecules.get(i).getState());
										dt.setModulator(containingMolecules.get(i).isModulator());
										dt.setModificationSites(((BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject()).getModificationSites());
										dt.setNumberOfModifSites(((BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject()).getNumberOfModifSites());
										if(containingMolecules.get(i).getName().startsWith("#")){
											cell=new ENucleicAcidFeatureCell(dt);
											dt.setMoleculeType("NucleicAcidFeature");
										}else{
											cell=new EMacromoleculeCell(dt);
											dt.setMoleculeType("Macromolecule");
										}		
										userObject.getGraph().getGraphLayoutCache().insert(c);	
										
										dt=null;
									}
									if(c!=null){						
											BiochamEntityData data=(BiochamEntityData)c.getUserObject();
											setGradientColor(data);						
										if(i>0){
											y0+=(containingMolDimension.height);
											//if(i==1){
												//y0+=(containingMolDimension.height);
											//}else{
											if(containingMolecules.get(i-1).getCardinality()>1){
												if(containingMolecules.get(i-1).getMoleculeType().contains("Complex")){
													//y0+=d.height*((BiochamEntityData)containingMolecules.get(i-1).getInstance().getUserObject()).getContainingMolecules().size()+23;	
													y0+=8;
												}else{										
													if(card>1){
														y0+=10;
													}else{
														y0+=3;
													}
												}
											}else{
												if(containingMolecules.get(i-1).getMoleculeType().contains("Complex")){
													y0+=6;
//													/y0+=d.height+8;//*((BiochamEntityData)containingMolecules.get(i-1).getInstance().getUserObject()).getContainingMolecules().size()+8;	
												}else{
													if(card>1){
														y0+=5;
													}else{
														y0+=1;
													}
												}
											}
												
											//}
										}	
										int w1=0;
										if (gradientColor != null && !preview) {										
											 card=containingMolecules.get(i).getCardinality();
											 String nnmm=containingMolecules.get(i).getName();
											//System.out.println("card="+card+",name="+((BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject()).getMultimerCardinality()+",name="+containingMolecules.get(i).getRepresentingName());
											
											
											// THE CONTAINING MOLECULE IS A MULTIMER........
											if(containingMolecules.get(i).getCardinality()>1){
												
												
												if(i>0 && (containingMolecules.get(i-1).getCardinality()>1 && containingMolecules.get(i-1).getUserObject().modificationSites!=null)){												
													y0+=15;
												}else if(i>0 && containingMolecules.get(i-1).getCardinality()>1){
													y0+=15;
												}else if(i>0 && containingMolecules.get(i-1).getUserObject().modificationSites!=null){
													y0+=10;
												}else if(i>0 && card>1){
													y0+=12;
												}else if(i==0 && containingMolecules.get(i).getMoleculeType().contains("Complex")){
													y0+=10;
												}else if(i==0){
													y0+=10;
													x0+=15;
												}
													
													/*else if(i>0 && containingMolecules.get(i-1).getCardinality()<=1){
												}
													y0+=13;
													
												}*/else{
													y0+=5;
												}									
												//BiochamEntityData dt=(BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject();
												double mw=data.getSize().getWidth();
												double mh=data.getSize().getHeight();									
												//System.out.println("**** mw="+mw+",mh="+mh);									
												int k=data.getRepresentingName().length()*2;
												//d.width+=k;									
												//System.out.println("**** Name:"+data.getRepresentingName()+", k="+k);
												g2.setClip(x0,y0,d.width+100,d.height+100);
												g2.setPaint(Color.GRAY);
												//draw a multimer......
												int set=0;									
												if(containingMolecules.get(i).getMoleculeType().contains("Nucleic")){																			
													if(((BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject()).getColor()!=null){
														gradientColor=((BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject()).getColor();
													}else{
														gradientColor=GraphUtilities.NUCLEIC_ACID_FEATURE_COLOR;
													}
													drawContainingNucleicMultimer(g, g2, x0, y0, d.width, d.height);										
												}else if(containingMolecules.get(i).getMoleculeType().contains("Complex")){										
													
													
													
													if(((BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject()).getColor()!=null){
														gradientColor=((BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject()).getColor();
													}else{
														gradientColor=GraphUtilities.COMPLEX_COLOR;
													}
													
												
													drawComplexSymbolWithBorder(g2,x,y, d.width-z, d.height+5, pntColor);
													
													g2.setPaint(gradientColor);
													//fillComplex(g2,0,0,d.width,d.height);								
													g2.setPaint(Color.black);
													g.drawString(data.getRepresentingName(),d.width/2-g.getFontMetrics().stringWidth(data.getRepresentingName())/2,20*d.height/21 );
													try {
														setBorder(null);
														setOpaque(false);
														selected = false;
														super.paint(g);
													} finally {
														selected = tmp;
													}
													bordercolor=Color.black;;
													setBorder(BorderFactory.createRaisedBevelBorder());
													if (bordercolor != null) {
														g.setColor(bordercolor);
														g2.setStroke(new BasicStroke(b));
														//drawComplex(g2,b / 2,b / 2,d.width,d.height);
														                       
													}	
													g2.setPaint(gradientColor);									
													fillComplex(g2,x0+10,y0+5,d.width-5, d.height);
													g2.setPaint(Color.black);
													drawComplex(g2,x0+10,y0+5,d.width-5, d.height);
													g2.setPaint(gradientColor);
													fillComplex(g2,x0,y0,d.width, d.height);
													g2.setPaint(Color.black);
													drawComplex(g2,x0,y0,d.width, d.height);
													set=1;
												}else{																
												
													if(((BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject()).getColor()!=null){
														gradientColor=((BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject()).getColor();
													}else{
														gradientColor=GraphUtilities.MACROMOLECULE_COLOR;
													}
													drawContainingMacromoleculeMultimer(g, g2, b, x0, y0, d.width, d.height);											
												}														
												if(set==0){
//													draw the box of cardinality....									
													g2.setPaint(Color.GRAY);				
													g.setClip(x0-20,y0-30,d.width+60,(int)maxHeight+d.height*2+20);				
													String cardinality="N:"+Integer.toString(containingMolecules.get(i).getCardinality());
													if(containingMolecules.get(i).getCardinality()/10<1){
														g2.drawRect(x0+4,y0-5,25,13);
													}else{
														g2.drawRect(x0+4,y0-5,38,13);							
													}
													g2.drawString(cardinality,x0+6,y0+6);								
													g2.setPaint(Color.black);
													cardinality=null;										
													if(data.getMoleculeState()==MoleculeState.MODIFIED){											
														g.setClip(x0-50,y0-50,d.width+400,d.height+200);
														g2.setStroke(new BasicStroke(1));			
														Font f0=g.getFont();
														Font f1=new Font(f0.getFamily(),Font.PLAIN,10);
														g.setFont(f1);
														g.drawOval(x0+d.width-2, y0+2,12,12);
														g.drawString("m",x0+d.width,y0+11);
														if(data.getModificationSites()!=null){
															FontRenderContext frc = new FontRenderContext(null, false, false);	
															int len=(int)g.getFont().getStringBounds(data.getModificationSites(), frc).getWidth();
															int min_0=len;
															if(data.getModificationSites().length()<2){
																min_0+=2;
															}else if(len>=d.width-20){
																min_0-=36*(len-d.width)/37+data.getNumberOfModifSites()*2+5;													
															}
															//System.out.println("\n\n\n COMPLEX="+userObject.getName()+",lenSites="+len+",min_0="+min_0+",d.width="+d.width);
																											
															 f1=new Font(f0.getFamily(),Font.PLAIN,9);
															 g2.setFont(f1);
															g2.setPaint(Color.black);		
														
															g.drawString(data.getModificationSites(),d.width-(min_0-d.width/5),y0+17);
															g2.setFont(f0);
															f1=null;	
															/*FontRenderContext frc = new FontRenderContext(null, false, false);	
															int len=(int)g.getFont().getStringBounds(data.getModificationSites(), frc).getWidth();
															int min_0=len;
															if(len>=d.width-25){
																min_0-=36*(len-d.width)/37+data.getNumberOfModifSites()*5+5;												
															}
															if(data.getNumberOfModifSites()==1){
																if(data.getModificationSites().length()<5){
																	min_0+=1;	
																}else{
																	min_0-=(complexWidth/10);
																}
															}else if(data.getNumberOfModifSites()==2){
																min_0-=complexWidth/10;
															}else{
																min_0-=(complexWidth/20)-10;
															}		
															Font f0=g.getFont();
															Font f1=new Font(f0.getFamily(),Font.PLAIN,9);
															g.setFont(f1);
															g2.setPaint(Color.black);		
														
															g.drawString(data.getModificationSites(),d.width-min_0+10,y0+17);
															g2.setFont(f0);
															f1=null;			*/
															/*FontRenderContext frc = new FontRenderContext(null, false, false);	
															int len=(int)g.getFont().getStringBounds(data.getModificationSites(), frc).getWidth();
															int min_0=len;
															if(data.getModificationSites().length()<2){
																min_0+=2;
															}else if(len>=d.width-20){
																min_0-=36*(len-d.width)/37+data.getNumberOfModifSites()*2+5;													
															}
															//System.out.println("\n\n\n COMPLEX="+userObject.getName()+",lenSites="+len+",min_0="+min_0+",d.width="+d.width);
															if(len>=d.width-10){
																//System.out.println("\n\n COMPLEX*************************before="+userObject.getSize().width);
																if(Math.abs(userObject.getSize().width-(d.width+(len-d.width)/4))>5){
																	userObject.getSize().width=d.width+(len-d.width)/4;		
																	userObject.applySize();		
																}			
															}
															int min_0=data.getModificationSites().length()*7+d.width/(data.getNumberOfModifSites()*5);//(10-userObject.getNumberOfModifSites())+d.width/10;
															min_0+=8*((min_0/data.getNumberOfModifSites())/40);
															System.out.println("\n\nContainingMolecule-COMPLEX adding="+d.width/(data.getNumberOfModifSites()*40)+"sites="+data.getNumberOfModifSites()+", length="+data.getModificationSites().length()+"**MACROMOLECULE**len="+min_0+"\n****d.width="+d.width);
															if(min_0>150){
																min_0+=30;
															}
															if(min_0>=d.width+20){
																System.out.println("\n\nContainingMolecule-COMPLEX ****before="+data.getSize().width);
																if(data.getSize().width<=d.width){
																	data.getSize().width+=17;							
																}						
																data.applySize();					
															}
															
															Font f0=g.getFont();
															Font f1=new Font(f0.getFamily(),Font.PLAIN,10);
															g.setFont(f1);
															g.setColor(Color.black);
															g.drawString(data.getModificationSites(),d.width-min_0,y0+15);	
															g.setFont(f0);
															f1=null;*/
														}		
														//int min=userObject.getNumberOfModifSites()*20;
														//g.drawString(data.getModificationSites(),d.width-min,-y0+15);			
													}										
												}else{
//													draw the box of cardinality....									
													g2.setPaint(Color.GRAY);				
													g.setClip(x0-20,y0-30,d.width+60,(int)maxHeight+d.height*2);		
													g2.setStroke(new BasicStroke(1));	
													String cardinality="N:"+Integer.toString(containingMolecules.get(i).getCardinality());
													if(containingMolecules.get(i).getCardinality()/10<1){
														g2.drawRect(x0+14,y0-13,25,12);
													}else{
														g2.drawRect(x0+14,y0-13,38,12);							
													}
													g2.drawString(cardinality,x0+16,y0-1);								
													g2.setPaint(Color.black);
													cardinality=null;										
													if(data.getMoleculeState()==MoleculeState.MODIFIED){											
														g.setClip(x0-10,y0-10,d.width+400,d.height+60);
														g2.setPaint(Color.BLACK);
														g2.setStroke(new BasicStroke(1));	
														Font f0=g.getFont();
														Font f1=new Font(f0.getFamily(),Font.PLAIN,10);
														g.setFont(f1);
														g.drawOval(x0+d.width-6, y0-10,12,12);
														g.drawString("m",x0+d.width-4,y0);	
														FontRenderContext frc = new FontRenderContext(null, false, false);	
														int len=(int)g.getFont().getStringBounds(data.getModificationSites(), frc).getWidth();
														int min_0=len;
														if(data.getModificationSites().length()<2){
															min_0+=2;
														}else if(len>=d.width-20){
															min_0-=36*(len-d.width)/37+data.getNumberOfModifSites()*2+5;													
														}
														//System.out.println("\n\n\n COMPLEX="+userObject.getName()+",lenSites="+len+",min_0="+min_0+",d.width="+d.width);
																												
														f1=new Font(f0.getFamily(),Font.PLAIN,9);
														g.setFont(f1);
														g2.setPaint(Color.black);		
													//(min_0-d.width/5)
														g.drawString(data.getModificationSites(),d.width-(min_0-d.width/5),y0+11);
														g2.setFont(f0);
														f1=null;	
														/*FontRenderContext frc = new FontRenderContext(null, false, false);	
														int len=(int)g.getFont().getStringBounds(data.getModificationSites(), frc).getWidth();
														int min_0=len/2;
														//int min_0=1*d.width/60;//data.getModificationSites().length()*2+d.width/(data.getNumberOfModifSites()*5);//(10-userObject.getNumberOfModifSites())+d.width/10;4
														//min_0+=6*((min_0/data.getNumberOfModifSites())/40);
													//	System.out.println("\n\nContainingMolecule-COMPLEX adding="+d.width/(data.getNumberOfModifSites()*40)+"sites="+data.getNumberOfModifSites()+", length="+data.getModificationSites().length()+"**MACROMOLECULE**len="+min_0+"\n****d.width="+d.width);
														//if(min_0>150){
															//min_0+=30;
														//}
														//min_0-=80;
														if(min_0>=d.width+20){
															//System.out.println("\n\nContainingMolecule-COMPLEX ****before="+data.getSize().width);
															if(data.getSize().width<=d.width){
																data.getSize().width+=17;							
															}						
															data.applySize();					
														}
														
														//g2.setPaint(new Color(135,135,135));//96,123,139));//131,139,131));//0,128,128));//85,85,85));
														Font f0=g.getFont();
														Font f1=new Font(f0.getFamily(),Font.PLAIN,10);
														g.setFont(f1);
														g.setColor(Color.black);
														g.drawString(data.getModificationSites(),d.width-min_0/4,y0+10);	
														//int min=userObject.getNumberOfModifSites()*20;
														//g.drawString(data.getModificationSites(),d.width-min,y0+6);	
														g.setFont(f0);
														f1=null;*/
													}	
												}								
												
												//draw the name if the containing molecule is a multimer .......
												g2.setPaint(Color.black);
												g.setColor(Color.black);
												drawContainingMultimerName(g, x0, y0, data,d.width, d.height);									
											
																	
												
												
											//THE CONTAINING MOLECULE IS NOT A MULTIMER........
											}else{									
												
												//if yhe previous was a multimer, let a space for the next one....
												/*if(i>0 && containingMolecules.get(i-1).getCardinality()>1){
													y0+=10;
												}*/		
												if(i>0 && (containingMolecules.get(i-1).getCardinality()>1 && containingMolecules.get(i).getUserObject().getModificationSites()!=null)){
													y0+=10;
													//y0+=15;
												}else if(i>0 && containingMolecules.get(i-1).getCardinality()>1){
													y0+=10;
												}else if(i>0 && containingMolecules.get(i).getUserObject().getModificationSites()!=null){
													y0+=10;
												}else if(i>0 && containingMolecules.get(i-1).getUserObject().getModificationSites()!=null){
													y0+=5;
												}
													
													/*else if(i>0 && containingMolecules.get(i-1).getCardinality()<=1){
												}
													y0+=13;
													
												}*/else{
													y0+=5;
												}			
											//	double mw=data.getSize().getWidth();
											//	double mh=data.getSize().getHeight();									
												//System.out.println("**** mw="+mw+",mh="+mh);									
											//	int k=data.getRepresentingName().length()*2;									
												//System.out.println("**** Name:"+data.getRepresentingName()+", k="+k);									
												int set=0;
												g2.setPaint(gradientColor);
												
												if(GraphUtilities.getCellType(containingMolecules.get(i).getInstance()).contains("Nucleic")){
													FontRenderContext frc = new FontRenderContext(null, false, false);	
													w-=2;
													h-=2;
													int incHeight=0;
													if(data.getMoleculeState()==MoleculeState.MODIFIED){	
														if(data.getModificationSites()!=null){
															
															int len=(int)g.getFont().getStringBounds(data.getModificationSites(), frc).getWidth();
															
															if(len>=d.width-5){
																//System.out.println("*****************ENLARGING THE CONTAINING MOLECULE "+data.getName()+", for lenModifSites="+len+",d.width="+d.width);												
																if(Math.abs(data.getSize().width-(d.width+(len-d.width)/4))>5){
																	w1=3*(len-d.width)/4+5;											
																}										
															}else{
																w1=0;
															}											
															incHeight+=5;
															//System.out.println("ENLARGING THE CONTAINING MOLECULE"+data.getName()+" width="+d.width+"w1="+w1+","+data.getName()+" for "+w1);
															//System.out.println("\n...and the COMPLEX....");
															g2.setClip((int)g2.getClipBounds().getX(),(int)g2.getClipBounds().getY(),(int)g2.getClipBounds().getWidth(),(int)g2.getClipBounds().getHeight());
														
														}
													}
													if(d.width+w1>maxLen){
														maxLen=d.width+w1;
													}
													int nameLen=(int)g.getFont().getStringBounds(data.getRepresentingName(), frc).getWidth();
													if(nameLen+20>=(d.width+w1)){
														w1+=nameLen-(d.width+w1)/2-20;
													}	
													if(((BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject()).getColor()!=null){
														gradientColor=((BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject()).getColor();
													}else{
														gradientColor=GraphUtilities.NUCLEIC_ACID_FEATURE_COLOR;
													}
													drawGeneSymbol(g, g2, x0,y0+8,d.height+incHeight, d.width+w1, gradientColor);		
													
												
																					
												}else if(containingMolecules.get(i).getMoleculeType().contains("Complex")){
																		
													
													data=containingMolecules.get(i).getUserObject();
													int incHeight=5;										
													if(data.getMoleculeState()==MoleculeState.MODIFIED){	
														if(data.getModificationSites()!=null){
															
															int len=(int)g.getFont().getStringBounds(data.getModificationSites(), GraphUtilities.frc).getWidth();
															
															if(len>=d.width-10){
																//System.out.println("1111111*****************ENLARGING THE CONTAINING MOLECULE "+data.getName()+", for lenModifSites="+len+",d.width="+d.width);												
																if(Math.abs(data.getSize().width-(d.width+(len-d.width)/4))>5){
																	w1=3*(len-d.width)/7;//+len/8;											
																}										
															}else{
																w1=0;
															}											
															//incHeight+=85;
															//System.out.println("*******11111111***********ENLARGING THE CONTAINING MOLECULE"+data.getName()+" width="+d.width+"w1="+w1+","+data.getName()+" for "+w1);
															//System.out.println("\n...and the COMPLEX....");
															g2.setClip((int)g2.getClipBounds().getX(),(int)g2.getClipBounds().getY(),(int)g2.getClipBounds().getWidth(),(int)g2.getClipBounds().getHeight());
														
														}
													}
													if(d.width+w1>maxLen){
														maxLen=d.width+w1;
													}
													int nameLen=(int)g.getFont().getStringBounds(data.getRepresentingName(), GraphUtilities.frc).getWidth();
													if(nameLen+10>=(d.width+w1)){
														w1+=(nameLen-(d.width+w1)/2);
														
													}
													//w+=w1;
													if(data.getModificationSites()!=null){
														int len=(int)g.getFont().getStringBounds(data.getModificationSites(), GraphUtilities.frc).getWidth();
														if(len>(d.width+w1)-10){
															w1+=(len-(d.width+w1))/2;	
														}
													}
													//h=d.height;
													//h+=incHeight;
													if(((BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject()).getColor()!=null){
														gradientColor=((BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject()).getColor();
													}else{
														gradientColor=GraphUtilities.COMPLEX_COLOR;														
													}
													g2.setClip(x0-50,y0-50,w+150,h+150);
													g2.setPaint(gradientColor);
													fillComplex(g2,x0,y0+10,d.width+w1,d.height+incHeight);
													g2.setPaint(Color.gray);
													drawComplex(g2,x0,y0+10,d.width+w1,d.height+incHeight);
													
													set=1;
												}else{	
													FontRenderContext frc = new FontRenderContext(null, false, false);	
													int incHeight=0;
													if(data.getMoleculeState()==MoleculeState.MODIFIED){	
														if(data.getModificationSites()!=null){
															
															int len=(int)g.getFont().getStringBounds(data.getModificationSites(), frc).getWidth();
															
															if(len>=d.width-5){
																//System.out.println("*****************ENLARGING THE CONTAINING MOLECULE "+data.getName()+", for lenModifSites="+len+",d.width="+d.width);												
																if(Math.abs(data.getSize().width-(d.width+(len-d.width)/4))>5){
																	w1=3*(len-d.width)/4+5;											
																}										
															}else{
																w1=0;
															}											
															incHeight+=5;
															//System.out.println("ENLARGING THE CONTAINING MOLECULE"+data.getName()+" width="+d.width+"w1="+w1+","+data.getName()+" for "+w1);
															//System.out.println("\n...and the COMPLEX....");
															g2.setClip((int)g2.getClipBounds().getX(),(int)g2.getClipBounds().getY(),(int)g2.getClipBounds().getWidth(),(int)g2.getClipBounds().getHeight());
														
														}
													}
													if(d.width+w1>maxLen){
														maxLen=d.width+w1;
													}
													int nameLen=(int)g.getFont().getStringBounds(data.getRepresentingName(), frc).getWidth();
													if(nameLen+20>=(d.width+w1)){
														w1+=nameLen-(d.width+w1)/2-20;
													}
													if(((BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject()).getColor()!=null){
														gradientColor=((BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject()).getColor();
													}else{
														gradientColor=GraphUtilities.MACROMOLECULE_COLOR;														
													}
													g2.setPaint(gradientColor);
													g2.setClip(x0,y0,(d.width+w1)+200,d.height+200);
													drawMacromoleculeSymbol(g2, b, d.width+w1,d.height+incHeight, x0, y0);
												}					
												if(set>0){
													if(data.getMoleculeState()==MoleculeState.MODIFIED && data.getModificationSites()!=null){											
														g.setClip(x0-50,y0-50,(d.width+w1)+400,d.height+200);
														g2.setPaint(Color.BLACK);
														g2.setStroke(new BasicStroke(1));												
														Font f0=g.getFont();
														Font f1=new Font(f0.getFamily(),Font.PLAIN,10);
														g.setFont(f1);
														int skrati=7;
														if(d.height<70){
															skrati=4;
														}	
														g.drawOval(x0+(d.width+w1)-d.height/skrati, y0+1,12,12);
														g.drawString("m",x0+(d.width+w1)-d.height/skrati+2,y0+11);	
														g.setFont(f0);
														f1=null;
														
														FontRenderContext frc = new FontRenderContext(null, false, false);	
														int len=(int)g.getFont().getStringBounds(data.getModificationSites(), frc).getWidth();
														int min_0=len/2;
														
														
														skrati=w/200;
														//min_0+=skrati/2;
														
														f1=new Font(f0.getFamily(),Font.PLAIN,9);
														g.setFont(f1);
														g2.setPaint(Color.black);	
														g.drawString(data.getModificationSites(),(d.width+w1)-min_0,y0+20);	
														g2.setFont(f0);
														f1=null;
													}	
												}else{
													if(data.getMoleculeState()==MoleculeState.MODIFIED && data.getModificationSites()!=null){											
														g.setClip(x0-50,y0-50,(d.width+w1)+400,d.height+200);
														g2.setStroke(new BasicStroke(1));										
														g.drawOval(x0+(d.width+w1)-2, y0,12,12);
														Font f0=g.getFont();
														Font f1=new Font(f0.getFamily(),Font.PLAIN,10);
														g.setFont(f1);
														g.drawString("m",x0+(d.width+w1),y0+10);
														g.setFont(f0);
														f1=null;
														FontRenderContext frc = new FontRenderContext(null, false, false);	
														int len=(int)g.getFont().getStringBounds(data.getModificationSites(), frc).getWidth();
														int min_0=len;
														if(len>=d.width-25){
															min_0-=36*(len-d.width)/37+data.getNumberOfModifSites()*5+5;												
														}
														if(data.getNumberOfModifSites()==1){
															if(data.getModificationSites().length()<5){
																min_0+=1;	
															}else{
																min_0-=(complexWidth/10);
															}
														}else if(data.getNumberOfModifSites()==2){
															min_0-=complexWidth/10;
														}else{
															min_0-=(complexWidth/20)-10;
														}													
														f1=new Font(f0.getFamily(),Font.PLAIN,9);
														g.setFont(f1);
														g2.setPaint(Color.black);		
													
														g.drawString(data.getModificationSites(),d.width-min_0,y0+17);
														g2.setFont(f0);
														f1=null;											
													}	
												}
											}
										}												
										
										try {
											setBorder(null);
											setOpaque(false);
											selected = false;
											super.paint(g);
										} finally {
											selected = tmp;
										}		
										
										//draw the name if the containing molecule is a Complex............
										int add=0;
										if(data.getMoleculeState()==MoleculeState.MODIFIED){	
											add=8;
										}
										Font f0=g.getFont();
										Font f1=new Font(f0.getFamily(),Font.BOLD,f0.getSize());
										g.setColor(Color.black);
										g.setFont(f1);
										//System.out.println("\n\n\n->->->->->->->->->->->->->->->->->->C_MOLECULENAME="+data.getRepresentingName()+",len="+g.getFontMetrics().stringWidth(data.getRepresentingName()));
										if(data.getRepresentingName()!=null && data.getMultimerCardinality()<=1){
											
											if(data.getRepresentingName().contains("-")){	
												int lenName=g.getFontMetrics().stringWidth(data.getRepresentingName());
												g.drawString(data.getRepresentingName().trim(),x0+(d.width+w1)/2-lenName/2,y0+d.height+add+5);//y0+g.getFontMetrics().getHeight()/2+decide*d.height+20+add);
											}else{
												//g.drawString(userObject.getRepresentingName(),d.width/2-g.getFontMetrics().stringWidth(userObject.getRepresentingName())/2,d.height/2 +g.getFontMetrics().getHeight()/5+add);
												
												int lenName=g.getFontMetrics().stringWidth(data.getRepresentingName());
												if(lenName==1){
													g.drawString(data.getRepresentingName().trim(),x0+(w1/2+d.width)/2,y0+d.height/2+g.getFontMetrics().getHeight()/2+5+add );
												//}//else if(lenName>2 && lenName<7){
													//g.drawString(data.getRepresentingName().trim(),x0-lenName*2+d.width/2-g.getFontMetrics().stringWidth(data.getRepresentingName())/2+data.getRepresentingName().length()*2,y0+d.height/2+g.getFontMetrics().getHeight()/2+5+add );
												}else{
													g.drawString(data.getRepresentingName().trim(),x0+(d.width+w1)/2-lenName/2,y0+d.height/2+g.getFontMetrics().getHeight()/2+5+add );
													//g.drawString(data.getRepresentingName().trim(),x0+d.width/2-g.getFontMetrics().stringWidth(data.getRepresentingName())/2+data.getRepresentingName().length(),y0+d.height/2+g.getFontMetrics().getHeight()/2+5+add );
												}
											}								
										}
										f1=null;
										data=null;
									}							
									c=null;
							}
						}
					}
					/**....ELSE Show just complex name....**///				
				}
			}
			Position pos=userObject.getPosition();
			
			if(pos!=null){
				
				Rectangle2D bnds=BiochamGraphConstants.getBounds(((DefaultGraphCell)cell).getAttributes());
				if(bnds!=null){
					
					double xc=pos.getX();
					double yc=pos.getY();
					double xx=bnds.getX();
					double yy=bnds.getY();
					EComplexCell iv=(EComplexCell)cell;
					iv.setX(xx);
					iv.setY(yy);
					pos.setX(xx);
					pos.setY(yy);
				}
			}			
		}

		public void devineUserObjectData(BiochamEntityData dt){
		
			
			
			
		}


		/**
		 * @param g
		 * @param g2
		 * @param d
		 * @param userObject
		 * @param w
		 * @return
		 */
		private int drawComplexModificationSites(Graphics g, Graphics2D g2, Dimension d, BiochamEntityData userObject, int w) {
			int maxLen;
			g.setClip(-20,-20,d.width+340,d.height+40);
			g2.setPaint(Color.BLACK);
			g2.setStroke(bs1);
			int yy=15,y1=5;
			if(d.width>190){
				y1=4;
			}
			int skrati=7;
			if(d.height<50){
				skrati=4;
			}	
			g.drawOval(d.width-d.height/skrati, -yy+3,16,16);				
			g.drawString("m",d.width-d.height/skrati+3,-y1+5);													
			int len=(int)g.getFont().getStringBounds(userObject.getModificationSites(), GraphUtilities.frc).getWidth();
			maxLen=len;
			int maxNumOfSites=userObject.getNumberOfModifSites();
			int maxWidthIndex=-1;
			for(int k=0;k<userObject.getContainingMolecules().size();k++){
				if(userObject.getContainingMolecules().get(k).getUserObject().getModificationSites()!=null){
					int l0=(int)g.getFont().getStringBounds(userObject.getContainingMolecules().get(k).getUserObject().getModificationSites(), GraphUtilities.frc).getWidth();
					if(maxLen<l0){
						maxLen=l0;
						maxWidthIndex=k;
					}
					if(maxNumOfSites<userObject.getContainingMolecules().get(k).getUserObject().getNumberOfModifSites()){
						maxNumOfSites=userObject.getContainingMolecules().get(k).getUserObject().getNumberOfModifSites();
					}
				}							
			}
			int min_0=len;		
			if(userObject.getModificationSites().length()<5){
				min_0+=15;
			}else if(len>=d.width-20){							
				min_0-=36*(len-d.width)/37+userObject.getNumberOfModifSites()*2+13;							
			}
			if(userObject.getNumberOfModifSites()==1){
				min_0+=len/8+d.width/30;
			}
			skrati=d.width/15;
			min_0+=skrati-len/15;
			
			Font f0=g.getFont();						
			Font f1=new Font(f0.getFamily(),Font.PLAIN,10);
			g.setFont(f1);	
			g2.setPaint(Color.black);
			g.drawString(userObject.getModificationSites(),d.width-min_0,-y1+20);						
			g.setFont(f0);					
			f1=null;f0=null;
			return maxLen;
		}



		/**
		 * @param g
		 * @param g2
		 * @param complexName
		 * @param h
		 * @param w
		 * @param pntColor
		 * @return
		 */
		private int drawComplex(Graphics g, Graphics2D g2, String complexName, int h, int w, GradientPaint pntColor) {
			int complexWidth;			
			g.setClip(-25,-25,w+40,h+40);							
			complexWidth=w;
			drawComplexSymbolWithBorder(g2,0,0, w, h, pntColor);
			g2.setPaint(Color.black);
			g.drawString(complexName,w/2-g.getFontMetrics().stringWidth(complexName)/2,h-g.getFontMetrics().getHeight()/4 );
			return complexWidth;
		}



		/**
		 * @param g
		 * @param g2
		 * @param b
		 * @param userObject
		 * @param complexName
		 * @param h
		 * @param w
		 * @param pntColor
		 */
		private void drawCopyOfAComplex(Graphics g, Graphics2D g2, int b, BiochamEntityData userObject, String complexName, int h, int w, GradientPaint pntColor) {
			w-=(int) (b * 1.5);	
			g2.setClip(0, 0, w+40, h+80);						
			drawComplexAsClone(g2,0,0, h, w, pntColor);					
			g2.setPaint(Color.WHITE);			
			g2.setClip(0, 0, w+10, h+10);
			int diff=0;
			if(h>160 && h<400){
				diff=2*h/60+5;	
			}else if(h<=160){
				diff=5;			
			}else{
				diff=2*h/60+10;
			}
			if(userObject.getCopyInstance()>=10){
				g.drawString(""+userObject.getCopyInstance(),w/2-8,17*h/18+diff );
			}else{
				g.drawString(""+userObject.getCopyInstance(),w/2-4,17*h/18+diff);
			}
			g2.setPaint(Color.black);
			g.drawString(complexName,w/2-g.getFontMetrics().stringWidth(complexName)/2,5*h/6 );
		}




		/**
		 * @param g
		 * @param d
		 * @param userObject
		 */
		private void drawComplexInitialConcentration(Graphics g, Dimension d, BiochamEntityData userObject) {
			if(userObject.getGraph().isShowInitialConcentration()){					
				g.drawString(userObject.getInitialConcentration(),d.width/2,-1);
			}
		}




		/**
		 * @param g
		 * @param g2
		 * @param d
		 * @param userObject
		 */
		private void drawComplexCardinality(Graphics g, Graphics2D g2, Dimension d, BiochamEntityData userObject) {
			g2.setPaint(Color.GRAY);						
			g.setClip(-20,-20,d.width+80,d.height+80);				
			String cardinality="N:"+Integer.toString(userObject.getMultimerCardinality());
			if(userObject.getMultimerCardinality()/10<1){
				g.drawRect(d.width/13,-16,24,15);						
			}else{
				g.drawRect(d.width/13,-16,31,15);
			}
			g.drawString(cardinality,d.width/13+2,-4);
		}




		/**
		 * @param g
		 * @param g2
		 * @param b
		 * @param d
		 * @param tmp
		 * @param x
		 * @param y
		 * @param z
		 * @param complexName
		 * @param h
		 * @param w
		 * @param pntColor
		 */
		private void drawComplexMultimer(Graphics g, Graphics2D g2, int b, Dimension d, boolean tmp, int x, int y, int z, String complexName, int h, int w, GradientPaint pntColor) {
			drawComplexSymbolWithBorder(g2,x,y, w-z, h- (int) (5*b * 1.5), pntColor);							
			g2.setPaint(new GradientPaint(0, 0, getBackground(),getWidth(), getHeight(), gradientColor, true));
			fillComplex(g2,0,0,d.width,d.height);								
			g2.setPaint(Color.black);
			String representingName=userObject.getRepresentingName();	
//			/g.drawString(complexName,w/2-g.getFontMetrics().stringWidth(complexName)/2,h-g.getFontMetrics().getHeight()/4 );
			g.drawString(representingName,d.width/2-g.getFontMetrics().stringWidth(representingName)/2,20*d.height/21 );
			try {
				setBorder(null);
				setOpaque(false);
				selected = false;
				super.paint(g);
			} finally {
				selected = tmp;
			}
			bordercolor=Color.black;;
			setBorder(BorderFactory.createRaisedBevelBorder());
			if (bordercolor != null) {
				g.setColor(bordercolor);
				g2.setStroke(new BasicStroke(b));
				drawComplex(g2,b / 2,b / 2,d.width- (int) (b * 1.5) - 1,d.height- (int) (b * 1.5));								                       
			}
		}




		/**
		 * @param g
		 * @param g2
		 * @param b
		 * @param userObject
		 * @param x
		 * @param y
		 * @param z
		 * @param complexName
		 * @param h
		 * @param w
		 * @param pntColor
		 */
		private void drawCopyOfComplexMultimer(Graphics g, Graphics2D g2, int b, BiochamEntityData userObject, int x, int y, int z, String complexName, int h, int w, GradientPaint pntColor) {
			drawComplexAsClone(g2,x,y, h, w-z, pntColor);	
			//the background..........																		
			//System.out.println("(x,y)_1="+rect.getX()+","+rect.getY());
			//rect=g2.getClipBounds();
			//System.out.println("(x,y)_2="+rect.getX()+","+rect.getY());
			g2.setClip(0, 0,w+20, h+20);
			//rect=g2.getClipBounds();
			//System.out.println("(x,y)_3="+rect.getX()+","+rect.getY());
			//the foreground........
			int t=h;							
			drawComplexAsClone(g2,0,0, t, w- (int) (b * 1.5), pntColor);						
			int diff=0;
			if(h>160 && h<400){
				diff=2*h/60+5;	
			}else if(h<=160){
				diff=5;			
			}else{
				diff=2*h/60+10;
			}							
			g2.setPaint(Color.WHITE);			
			g2.setClip(0, 0, w, h);
			if(userObject.getCopyInstance()>=10){
				g.drawString(""+userObject.getCopyInstance(),w/2-8,16*h/17+diff );
			}else{
				g.drawString(""+userObject.getCopyInstance(),w/2-4,16*h/17+diff);
			}								
			g2.setPaint(Color.black);
			g.drawString(complexName,w/2-g.getFontMetrics().stringWidth(complexName)/2,5*h/6 );
		}
		
	
		
		
		
		
		
		
		
		
		
		/**************************   HELPER RENDERER METHODS   ***********************************/
		private void drawMacromoleculeSymbol(Graphics2D g2, int b, int w, int h, int x0, int y0) {
			double rr=containingMolArc;
			g2.fillRoundRect(x0, y0+8, w- (int) (b * 1.5), h- (int) (3*b * 1.5), containingMolArc, containingMolArc);
			g2.setPaint(Color.black);
			g2.drawRoundRect(x0, y0+8, w- (int) (b * 1.5), h- (int) (3*b * 1.5), containingMolArc, containingMolArc);
		}
		private void drawContainingMultimerName(Graphics g, int x0, int y0, BiochamEntityData data, int w, int h) {
			if(data.getName()!=null ){
				int putNameDown=0;
				if(data.getMoleculeState()==MoleculeState.MODIFIED){
					if(data.getModificationSites()!=null){
						putNameDown=5;
					}
				}
				
				if(data.getRepresentingName().contains("-") && !data.getMoleculeType().equals("Complex")){										
					/*StringTokenizer stTemp=new StringTokenizer(data.getRepresentingName(),"-");
					String tokenBefore=stTemp.nextToken().trim();
					int counter=1;
					String name="";
					while(stTemp.hasMoreTokens()){
						String token=stTemp.nextToken().trim();
						if(token.equals(tokenBefore)){
							counter++;
						}else{
							if(counter>1){						
								name+=tokenBefore;
							}
							if(name!=""){
								name+="-"+tokenBefore;
							}else{
								name+=tokenBefore;
							}
							counter=1;
							tokenBefore=token;
						}
					}	
					if(counter>1){	
						name+=tokenBefore;												
					}else{
						name+="-"+tokenBefore;
					}											
					stTemp=null;
					tokenBefore=null;
					counter=0;*/
					g.drawString(data.getRepresentingName(),x0+w/2-g.getFontMetrics().stringWidth(data.getRepresentingName())/4,y0+h/2+g.getFontMetrics().getHeight()/2+putNameDown);
				}else if(data.getRepresentingName().contains("-") && data.getMoleculeType().equals("Complex")){													
					//String tmpStr=data.getRepresentingName().substring(data.getRepresentingName().lastIndexOf("-")+1,data.getRepresentingName().length());	
					//String name=data.getRepresentingName().substring(0,data.getRepresentingName().indexOf(tmpStr))+tmpStr;										
					g.drawString(data.getRepresentingName(),x0+15,y0+g.getFontMetrics().getHeight()/2+10+putNameDown);											
					//tmpStr=null;
					//name=null;
				}
				else{											
					g.drawString(data.getRepresentingName().trim(),x0+w/2-g.getFontMetrics().stringWidth(data.getRepresentingName())/2,y0+h/2+g.getFontMetrics().getHeight()/2 +4+putNameDown);
				}							
			}
		}
		private void drawContainingNucleicMultimer(Graphics g, Graphics2D g2, int x0, int y0, int w, int h) {
			drawGeneSymbol(g, g2, x0+15,y0+8+8,h-5, w-10, gradientColor);
			g.setClip(x0-15, y0+8-15, w+30, h+40);		
			drawGeneSymbol(g, g2, x0,y0+8,h-2, w,gradientColor);
		}
		private void drawContainingMacromoleculeMultimer(Graphics g, Graphics2D g2, int b, int x0, int y0, int w, int h) {
			int roundRectArc=EMacromoleculeView.getArcSize(w,h);
			g.setClip(x0-15, y0+8-15, w+40, h+60);
			g2.setPaint(gradientColor);
			g.fillRoundRect(x0+3, y0+13, w- (int) (b * 1.5), h, roundRectArc, roundRectArc);
			g2.setPaint(Color.black);										
			g.drawRoundRect(x0+3, y0+13, w- (int) (b * 1.5), h, roundRectArc, roundRectArc);										
			g2.setPaint(Color.black);
			g2.setStroke(new BasicStroke(1));		
			g.setClip(x0-10, y0-10, w+60, h+60);
			g2.setPaint(Color.black);
			g.drawRoundRect(x0-1, y0+7, w-4, h+1, roundRectArc, roundRectArc);		
			g2.setPaint(gradientColor);										
			g.fillRoundRect(x0,y0+8, w-5, h, roundRectArc, roundRectArc);
		}
		private void setGradientColor(BiochamEntityData data) {
		
				if(data.getColor()!=null){
					gradientColor=data.getColor();
				}else{
					if(data.isModulator()){	
						
					}else if(data.isModulator()){
						gradientColor=Color.RED.darker();
					}else{
						if(data.getMoleculeType().contains("Macromolecule")){										
							gradientColor=GraphUtilities.MACROMOLECULE_COLOR;						
						}else if(data.getMoleculeType().contains("Nucleic")){	
							gradientColor=GraphUtilities.NUCLEIC_ACID_FEATURE_COLOR;
						}else if(data.getMoleculeType().contains("Complex")){	
							gradientColor=GraphUtilities.COMPLEX_COLOR;
						}
					}
				}
		}
		private void drawComplexAsClone(Graphics2D g2, int x, int y, int h, int w, GradientPaint pntColor) {
			drawComplexSymbolWithBorder(g2,x,y, w, h,Color.gray);
			if(h>100){
				g2.setClip(x, y, w,h-15);	
			}else{
				g2.setClip(x, y, w,h-13);			
			}
			drawComplexSymbolWithBorder(g2, x,y,w,h,pntColor);
		}
		private void drawComplexSymbolWithBorder(Graphics2D g2, int x, int y, int w, int h, Paint pntColor) {
			g2.setPaint(pntColor);	
			fillComplex(g2,x,y,w,h);	
			g2.setPaint(Color.black);
			drawComplex(g2,x,y,w,h);
		}
		private void drawComplex(Graphics2D g2, int x, int y, int w, int h) {
			GeneralPath gp=new GeneralPath();
			int skrati=7;
			if(h<50){
				skrati=4;
			}			
			int c=h/skrati;			
			gp.moveTo(c+x,y);//A
			gp.lineTo(w-c+x,y);//B
			gp.lineTo(w+x,c+y);//C
			gp.lineTo(w+x,h-c+y);//D
			gp.lineTo(w-c+x,h+y);//E
			gp.lineTo(c+x,h+y);//F
			gp.lineTo(x,h-c+y);//G
			gp.lineTo(x,c+y);	//H
			gp.lineTo(c+x,y);//A
			gp.closePath();
			g2.draw(gp);
			
			gp=null;
		}
		private void fillComplex(Graphics2D g2, int x, int y, int w, int h) {
			GeneralPath gp=new GeneralPath();
			int skrati=7;
			if(h<50){
				skrati=4;
			}				
			int c=h/skrati;				
			gp.moveTo(c+x,y);//A
			gp.lineTo(w-c+x,y);//B
			gp.lineTo(w+x,c+y);//C
			gp.lineTo(w+x,h-c+y);//D
			gp.lineTo(w-c+x,h+y);//E
			gp.lineTo(c+x,h+y);//F
			gp.lineTo(x,h-c+y);//G
			gp.lineTo(x,c+y);	//H
			gp.lineTo(c+x,y);//A
			gp.closePath();			
			g2.fill(gp);			
			gp=null;			
		}		
		private void fillClone(Graphics2D g2, int x, int y, int w, int h) {
			GeneralPath gp=new GeneralPath();
			int c=w/9;			
			gp.moveTo(x,y);//H
		//	gp.lineTo(w-c+x,y);//B
			gp.lineTo(w+x,y);//C
			gp.lineTo(w+x,h-c+y);//D
			gp.lineTo(w-c+x,h+y);//E
			gp.lineTo(c+x,h+y);//F
			gp.lineTo(x,h-c+y);//G
			//gp.lineTo(x,y);	//H
			//gp.lineTo(c+x,y);//A
			gp.closePath();
			g2.fill(gp);			
			gp=null;	
		}
		private void drawComplexAsMultimer(Graphics g, int b, Graphics2D g2, Dimension d) {
			g2.setPaint(Color.GRAY);
			GeneralPath gp=new GeneralPath();
			int c=d.width/9;
			int deviation=7;
			int dww=d.width-deviation;
			int dhh=d.height-deviation;
			AffineTransform transform = g2.getTransform();
			AffineTransform rat = new AffineTransform();
		    rat.translate(deviation, deviation);		    
		    g2.transform(rat);
			//gp=new GeneralPath();
			gp.moveTo(c,0);//A
			gp.lineTo(dww-c,0);//B
			gp.lineTo(dww,c);//C
			gp.lineTo(dww,dhh-c);//D
			gp.lineTo(dww-c,dhh);//E
			gp.lineTo(c,dhh);//F
			gp.lineTo(0,dhh-c);//G
			gp.lineTo(0,c);	//H
			gp.lineTo(c,0);//A
			gp.closePath();		
			
			g2.fill(gp);		
			gp=null;
			
			g2.setTransform(transform);
			GradientPaint pntColor=new GradientPaint(0, 0, getBackground(),getWidth(), getHeight(), gradientColor, true);
			g2.setPaint(pntColor);			
			int dw=d.width-deviation;
			int dh=d.height-deviation;
			gp=new GeneralPath();
			gp.moveTo(c,0);//A
			gp.lineTo(dw-c,0);//B
			gp.lineTo(dw,c);//C
			gp.lineTo(dw,dh-c);//D
			gp.lineTo(dw-c,dh);//E
			gp.lineTo(c,dh);//F
			gp.lineTo(0,dh-c);//G
			gp.lineTo(0,c);	//H
			gp.lineTo(c,0);//A
			gp.closePath();
			g2.fill(gp);
						
			g2.setPaint(Color.GRAY);				
			g.setClip(-20,-20,d.width+20,d.height+20);
					
			String cardinality="N:"+Integer.toString(userObject.getMultimerCardinality());
			if(userObject.getMultimerCardinality()/10<1){
				g.drawRect(15,-14,24,15);				
			}else{
				g.drawRect(15,-14,31,15);
						
			}
			g.drawString(cardinality,17,-1);
			
			if(userObject.getGraph().isShowInitialConcentration()){				
				g.drawString(userObject.getInitialConcentration(),55,-1);
			}
			
			transform = null;
			rat = null;
			gp=null;
			cardinality=null;
			pntColor=null;
			
		}
		private void drawComplex(int b, Graphics2D g2, Dimension d) {
			GeneralPath gp;
			int c;
			GradientPaint gPaint=new GradientPaint(0, 0, getBackground(),getWidth(), getHeight(), gradientColor, true);
							
			gp=new GeneralPath();			
			c=d.width/9;			
			gp.moveTo(c,0);//A
			gp.lineTo(d.width-c,0);//B
			gp.lineTo(d.width,c);//C
			gp.lineTo(d.width,d.height-c);//D
			gp.lineTo(d.width-c,d.height);//E
			gp.lineTo(c,d.height);//F
			gp.lineTo(0,d.height-c);//G
			gp.lineTo(0,c);	//H
			gp.lineTo(c,0);//A
			gp.closePath();
			g2.setPaint(Color.black);			
			g2.setPaint(gPaint);
			g2.fill(gp);
			
			g2.setPaint(Color.GRAY);				
			g2.setClip(-20,-20,d.width+20,d.height+20);
			
			if(userObject.getGraph().isShowInitialConcentration()){			
				g2.drawString(userObject.getInitialConcentration(),17,-1);
			}
			
			gp=null;
			gPaint=null;
		}
		private DefaultGraphCell createMissingCell(BiochamEntityData userObject, int i, DefaultGraphCell c) {
			try{
				c=containingMolecules.get(i).getInstance();
				UUID id=UUID.randomUUID();
				BiochamEntityData ddt=((BiochamEntityData)c.getUserObject());
				ddt.setId(id);
				containingMolecules.get(i).setId(id);
				c.setUserObject(ddt);								
				id=null;
				ddt=null;								
			}catch(Exception ee){
			}					
			if(c==null){
				BiochamEntityData dt=new BiochamEntityData(userObject.getGraph());
				dt.setColor(containingMolecules.get(i).getColor());
				dt.setInitialConcentration(containingMolecules.get(i).getInitialConcentration());
				dt.setMultimerCardinality(containingMolecules.get(i).getCardinality());
				dt.setId(containingMolecules.get(i).getId());
				dt.setName(containingMolecules.get(i).getName());
				dt.setRepresentingName(containingMolecules.get(i).getRepresentingName());
				dt.setMoleculeState(containingMolecules.get(i).getState());
				dt.setModulator(containingMolecules.get(i).isModulator());
				dt.setModificationSites(((BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject()).getModificationSites());
				dt.setNumberOfModifSites(((BiochamEntityData)containingMolecules.get(i).getInstance().getUserObject()).getNumberOfModifSites());
				if(containingMolecules.get(i).getName().startsWith("#")){
					cell=new ENucleicAcidFeatureCell(dt);
					dt.setMoleculeType("NucleicAcidFeature");
				}else{
					cell=new EMacromoleculeCell(dt);
					dt.setMoleculeType("Macromolecule");
				}		
				userObject.getGraph().getGraphLayoutCache().insert(c);	
				
				dt=null;
			}
			return c;
		}
		public Dimension getPreferredSize() {				
			Dimension d = super.getPreferredSize();
			return d;
		}
	
	}

	
	
	
	
	
	/**************************   OTHER COMPLEX VIEW HELPER METHODS   ***********************************/
	private int calculateMaxWidth(ArrayList<ContainingMolecule> mols) {
		int max=0;
		for(int i=0;i<mols.size();i++){
			int len=mols.get(i).getRepresentingName().length()*BiochamGraphConstants.getFont(this.getAllAttributes()).getSize();
			if(len>max){
				max=len;
			}
		}
		return max;
	}
	public CellViewRenderer getRenderer() {
		return renderer;
	}
	public Rectangle2D getBounds() {
		return BiochamGraphConstants.getBounds(getAllAttributes());
	}
	public Point2D getPerimeterPoint(VertexView view, Point2D source, Point2D p) {
		Rectangle2D bounds = view.getBounds();
		double x = bounds.getX();
		double y = bounds.getY();
		double width = bounds.getWidth();
		double height = bounds.getHeight();
		double xCenter = x + width / 2;
		double yCenter = y + height / 2;
		double dx = p.getX() - xCenter; // Compute Angle
		double dy = p.getY() - yCenter;
		double alpha = Math.atan2(dy, dx);
		double xout = 0, yout = 0;
		double pi = Math.PI;
		double pi2 = Math.PI / 2.0;
		double beta = pi2 - alpha;
		double t = Math.atan2(height, width);
		if (alpha < -pi + t || alpha > pi - t) { // Left edge
			xout = x;
			yout = yCenter - width * Math.tan(alpha) / 2;
		} else if (alpha < -t) { // Top Edge
			yout = y;
			xout = xCenter - height * Math.tan(beta) / 2;
		} else if (alpha < t) { // Right Edge
			xout = x + width;
			yout = yCenter + width * Math.tan(alpha) / 2;
		} else { // Bottom Edge
			yout = y + height;
			xout = xCenter + height * Math.tan(beta) / 2;
		}
		return new Point2D.Double(xout, yout);
	}	
	private void drawGeneSymbol(Graphics g, Graphics2D g2,int x, int y, int h, int w, Paint color) {
		g2.setPaint(color);	
		g.setClip(x, y, w+20, h+20); 
		g.fillRoundRect(x, y,w,h ,15,15);
		g2.setPaint(Color.BLACK);						
		g.drawRoundRect(x, y,w,h ,15,15);
		g2.setPaint(color);
		g.setClip(x, y, w+20, h-h/4); 
		g.fillRect(x, y, w, h);
		g2.setPaint(Color.BLACK);						
		g.drawRect(x, y, w, h);
	}
	private void drawMultimerMacromolecule(Graphics g, int b, Graphics2D g2, int roundRectArc, int x, int y, int z, int w, int h, Paint p) {
		g2.setPaint(Color.black);
		g.drawRoundRect(x, y, w- z, h- (int) (5*b * 1.5), roundRectArc, roundRectArc);		
		g2.setPaint(p);
		g.fillRoundRect(x,y, w- z, h- (int) (5*b * 1.5), roundRectArc, roundRectArc);			
		g2.setPaint(p);
		g.fillRoundRect(0, 0, w- (int) (b * 1.5), h, roundRectArc, roundRectArc);
	}



}
