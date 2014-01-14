package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.WorkbenchToolBars;
import fr.inria.contraintes.biocham.dialogs.DialogPlotCustomize;
import fr.inria.contraintes.biocham.menus.CustomPopupBiochamPlot;
import fr.inria.contraintes.biocham.modelData.ParamTableInitConc;
import fr.inria.contraintes.biocham.modelData.SimulationView;
import fr.inria.contraintes.biocham.parsers.ParserCSV;
import fr.inria.contraintes.biocham.plotting.DataSet;
import fr.inria.contraintes.biocham.plotting.DatasetProperties;
import fr.inria.contraintes.biocham.plotting.PlotDataCSV;
import fr.inria.contraintes.biocham.plotting.PlotFileConvertion;
import fr.inria.contraintes.biocham.plotting.PlotProperties;
import fr.inria.contraintes.biocham.plotting.PlotsComparizonWindowUtils;
import fr.inria.contraintes.biocham.plotting.PltFile;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;
import freemarker.template.TemplateException;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.StringTokenizer;
import java.util.UUID;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SpringLayout;


/**
 * Class thats creates the the plotting area for visualizing the numerical simulation results done by the biocham stochastic or ode simulator.
 * 
 * @author Dragana Jovanovska  
 */ 
public class BiochamPlot extends JPanel implements ActionListener{

	
	
	   public ArrayList<DataSet> dataSetsList;
	   private PlotDataCSV plotData;
	   
	   private PlotCanvas drawingArea;
	   private PlotCaption caption;	
	   private JPanel captionPane;
	   	      
	   private BiochamModel model;
	   private JColorChooser cc;
	   private String name;   		 	  
	   private UUID index;
	   private String plotFile=null;
	   public boolean withoutMistakes;	   
	   BiochamPlot instance;
	   
	   
	   
	   /**
	    * Plot from numerical simulation
	    * 
	    */
	   public BiochamPlot(BiochamModel m) {		  
		   
			 super(new BorderLayout());
			 model=m;
		     name=m.getModelName();
		     initializePanel();		     
	   }	
	   
	   /**
	    * Plot from external file (CSV,EXCEL,PLOT file) in the case of plotting traces
	    * 
	    */
	   public BiochamPlot(BiochamModel m,String n) {
			  
			 super(new BorderLayout());			
			 model=m;
		     name=n;
		     initializePanel();
	   }
	   

	/**
	 * BiochamPlot main panel initialization
	 * 
	 */
	private void initializePanel() {
		
		 //setLayout(new BorderLayout());      
		 setBackground(Color.WHITE);			 		    
		 cc = new JColorChooser(); 	
		 dataSetsList=new ArrayList<DataSet>();
		 setIndex(UUID.randomUUID());
		 instance=this;	 
	}
	   
	   
	   
	 	   
	   public boolean drawPlot(final String inputFile){
		   		   
		 Plot plot = null;
		 try {
			 plot = parsePlotFile(inputFile);
		 } catch (FileNotFoundException e) {
			 // TODO Auto-generated catch block
			 e.printStackTrace();
		 } catch (IOException e) {
			 // TODO Auto-generated catch block
			 e.printStackTrace();
		 }
		 if(plot!=null){
			 applyPlotToScreen(plot);
			 return true;
		 }else{
			 return false;
		 }
	   }
	 
	   
	   /**
		 * @param inputFile
		 * @return
		 * @throws IOException
		 * @throws FileNotFoundException
		 */
		private Plot parsePlotFile(final String inputFile) throws IOException, FileNotFoundException {
				
			   String pltFile = null;			   
			   if(inputFile.endsWith(".csv") || inputFile.endsWith(".xls")){						  
				   ParserCSV csvParser=new ParserCSV();
				   PlotDataCSV csvData;				   
				   if(inputFile.endsWith(".csv")){
					   csvData=csvParser.parseCSVTextFile(inputFile);
				   }else{
					   csvData=csvParser.parseCSVExcelFile(inputFile);
				   }
				   setPlotData(csvData);
				   if(csvData!=null){
					   for(int i=0;i<csvData.getItems().size();i++){
						   int size=csvData.getXTicks().size();
						   	DataSet ds=new DataSet(csvData.getItems().get(i),csvData.getXTicks(),csvData.getItemsYValues(csvData.getItems().get(i)));
						   	dataSetsList.add(ds);		     
					   }
					   PltFile plt=new PltFile();			
					   try {
						   pltFile=plt.create(dataSetsList,model);
					   } catch (TemplateException e) {
						   // TODO Auto-generated catch block
						   e.printStackTrace();
					   }
					   setDataSetsList(dataSetsList);	
				   }
				   
			   }else if(inputFile.endsWith(".plot")){					   
				   
				   PlotFileConvertion conversion=new PlotFileConvertion();		  		   
				   pltFile=conversion.fromPlotToPlt(inputFile,model);	
				   setPlotData(conversion.getCsvData());
				   setPlotFile(conversion.getCsvFile());
				   setDataSetsList(conversion.getDataSetsList());						   
			   }
			   
			   if(pltFile!=null){
				   Plot plot=new Plot();
				   Utils.debugMsg(pltFile);
				   plot.read(new FileInputStream(pltFile));					   
				   plot.setTitle(name);	
				   plot.setName(name);
				   plot.setReuseDatasets(false);
				   plot.setColor(true);
				   plot.setGrid(true);
				   plot.setXLabel("Time");	
				   plot.setYRange(0,20);
				   plot.setBackground(Color.white);			   
				   return plot;
			   }else{
				   return null;
			   }
		}
		
		
		
		
		
		
		/**
		 * Adding the plot to the BiochamPlotArea
		 */
		private void applyPlotToScreen(Plot plot) {
			 
			
			
			
			 SpringLayout sp=new SpringLayout();
			 caption =new PlotCaption();
			 caption.setLayout(sp);
			 JCheckBox cb;
			 for(int i=0;i<plot.getNumDataSets();i++){			   	  
				
				 if(plot.getFormats().get(i).color!=null){
					 Color c=plot.getFormats().get(i).color;
					 dataSetsList.get(i).setColor(c);
				     plot.setColor(i,c);
				 }else{
					 Color c=plot.getColors().get(i%plot.getColors().size());
					 dataSetsList.get(i).setColor(c);		
					 plot.getFormats().get(i).color=c;
					 plot.setColor(i,c);
				 }
				 String n=dataSetsList.get(i).getLabelName();
				 cb=new JCheckBox(n);
				 cb.setName(n);
				 cb.setActionCommand(n);
				 cb.setBackground(Color.WHITE);
				 cb.setFont(new Font("",Font.PLAIN,10));
				 cb.setSelected(true);
				 cb.setForeground(dataSetsList.get(i).getColor());
				 cb.addItemListener(caption);
				 cb.addMouseListener(new MouseAdapter(){				    		  
		    		  public void mouseClicked(MouseEvent e) {				         		
		    			  if(e.getSource() instanceof JCheckBox){
		    				  JCheckBox cb=(JCheckBox)e.getSource();
			    			  if(e.getButton() == MouseEvent.BUTTON3){
				                  String dataset=cb.getName();
				                  int i=getDatasetIndex(dataset);					               
				                  caption.setColor(i, cc.showDialog(BiochamMainFrame.frame,"Choose a color",dataSetsList.get(i).getColor()));
				                  cb.setForeground(dataSetsList.get(i).getColor());					                
			    			  }
		    			  }
		               }		    		  
		    	  });
		    	  caption.add(cb);
		    	  plot.removeLegend(i);
			 }
			 plot.repaint();			 
			 sp.putConstraint(SpringLayout.NORTH, caption.getComponent(0), 35, SpringLayout.NORTH, caption);
			 sp.putConstraint(SpringLayout.WEST, caption.getComponent(0), 0, SpringLayout.WEST, caption);
			 for(int i=1;i<plot.getNumDataSets();i++){
				 sp.putConstraint(SpringLayout.NORTH, caption.getComponent(i), 3, SpringLayout.SOUTH, caption.getComponent(i-1));
				 sp.putConstraint(SpringLayout.WEST, caption.getComponent(i), 0, SpringLayout.WEST, caption);
			 }
			 int max=30,temp=0;
			 for(int i=0;i<caption.getComponentCount();i++){
				 if(caption.getComponent(i) instanceof JCheckBox){
					 temp=((JCheckBox)caption.getComponent(i)).getName().length();
					 if(temp>=max){
						 max=temp;
					 }
				 }
			 }
			 caption.setPreferredSize(new Dimension(max*7,caption.getComponentCount()*30));			
			 UniversalScrollPane jsp=new UniversalScrollPane(caption);
			 jsp.setBorder(null);
			 jsp.setSize(caption.getPreferredSize());
			 //jsp.setViewportView(caption);			 
			 captionPane = new JPanel();			 
			 captionPane.setLayout(new BorderLayout());
			 captionPane.setBackground(Color.white);  
			 captionPane.add(jsp,BorderLayout.CENTER);			 
			 drawingArea = new PlotCanvas(plot);	
			      
			 add(drawingArea,BorderLayout.CENTER);
			 add(captionPane,BorderLayout.EAST);													 
			 plot.setSize(drawingArea.getPreferredSize());
			 validate();
		}
	   
		
	   /**
	    * Inner class that represents the caption.
	   */
	   public class PlotCaption extends JPanel implements ItemListener{
		   		     
		   PlotCaption() {			   		
			   super();
			   setBackground(Color.WHITE);  			   
		   }			   
		   public void setColor(int i, Color c) {
			   
		         if (c != null & c != dataSetsList.get(i).getColor()) {
		        	 
		        	 dataSetsList.get(i).setColor(c);
		        	 drawingArea.getPlot().setColor(i,c);    	
		        	 drawingArea.getPlot().repaint();
		        	 validate();
		        	 repaint();
		         }
		   }

		   public void itemStateChanged(ItemEvent e) {
			
				if(e.getSource() instanceof JCheckBox){
					   	
					JCheckBox cb=(JCheckBox)e.getSource();				   	
					String dataset=cb.getName();
				   	Plot plot=drawingArea.getPlot();			   				
				   	int ind=getDatasetIndex(dataset);
				   	if(e.getStateChange()==ItemEvent.DESELECTED){
				   		plot.clear(ind,false);
					   		
				   	}else if(e.getStateChange()==ItemEvent.SELECTED){				  
				   		cb.setSelected(true);
				   		int dsIndex=getDatasetIndex(dataset);
				   		DataSet ds=getDataSetByName(dataset);
				   		if(ds!=null){
				   			for(int i=0;i<ds.getYValues().size();i++){
				   				plot.addPoint(dsIndex,Double.valueOf(ds.getXTicks().get(i)),Double.valueOf(ds.getYValues().get(i)),true);
				   			}
				   		}
				   	}
					plot.repaint();
				   	drawingArea.validate();
				}	
		   }
		   public JCheckBox getCheckBoxByName(String name){
			   for(int i=0;i<getComponents().length;i++){
				   if(getComponents()[i].getName().equals(name)){
					   return (JCheckBox)getComponents()[i];
				   }
			   }
			   return null;
		   }		   
	   }
	   
	   
	   
	   public class PlotCanvas extends JPanel implements ActionListener{
		  
		   Plot plot;
		   CustomPopupBiochamPlot menu;		   
		   PlotCanvas(Plot p) {
			   
			  plot=p;
			  menu=new CustomPopupBiochamPlot(this);
			  setBackground(Color.WHITE);
			  plot.addMouseListener(new MouseAdapter(){
				  public void mousePressed(MouseEvent ev) {
				        if (ev.isPopupTrigger()) {
				          menu.getPopup().show(ev.getComponent(), ev.getX(), ev.getY());
				        }
				      }
				      public void mouseReleased(MouseEvent ev) {
				        if (ev.isPopupTrigger()) {
				          menu.getPopup().show(ev.getComponent(), ev.getX(), ev.getY());
				        }
				      }
			  });
			  addComponentListener(new ComponentListener(){				
					public void componentResized(ComponentEvent e) {		
						try{
							if(drawingArea!=null){
								if(drawingArea.getPlot()!=null){
									drawingArea.getPlot().setSize(getSize().width-50,getSize().height-50);
								}
							}
							
						}catch(Exception ex){
							ex.printStackTrace();
						}
					}
					public void componentHidden(ComponentEvent e) {
					}
					public void componentMoved(ComponentEvent e) {
					}
					public void componentShown(ComponentEvent e) {
					}				
		   		  });
	   		  add(plot,BorderLayout.CENTER);  		
	   		 
		   }
		   
		   public Plot getPlot() {			 
				return plot;
		   }  	  

		
		public void savePlotImage() {
			
						
			String[] imageFormats=ImageIO.getWriterFormatNames();
			for(int i=0;i<imageFormats.length;i++){
				imageFormats[i]= imageFormats[i].toLowerCase();
			}
			java.util.Arrays.sort(imageFormats);
			int counter=0;
			for(int i=0,j=i+1;i<imageFormats.length-1;i++,j++){
				if(!imageFormats[i].equals(imageFormats[j])){
					counter++;
				}
			}				   
			Object[] possibilities = new String[counter+1];
			int i=0,j=0,k=0;
			for(i=0,j=i+1,k=0;i<imageFormats.length-1;i++,j++){
				if(!imageFormats[i].equals(imageFormats[j])){
					possibilities[k]=imageFormats[i];
					k++;
				}
			}
			possibilities[k]="CSV";			
			String s = (String)JOptionPane.showInputDialog(((SimulationView)model.getWhoPopupSimulation()).getParentFrame(),
					" \nChoose Save Format:\n",
					"Save Image As",
					JOptionPane.PLAIN_MESSAGE,
					Icons.icons.get("File-1-48x48.png"+1.5),
					possibilities,
					"png");
		    if(s!=null){
				if(s.equals("CSV") && getPlotFile()==null){
		    		   
		    		   JOptionPane.showMessageDialog(((SimulationView)model.getWhoPopupSimulation()).getParentFrame(), "You can't save a trace file in a CSV format. It's already in a CSV format.","Warning",JOptionPane.INFORMATION_MESSAGE);
		    		   
		    	}else{
		    				    		
		    			String rep=Utils.showSaveDialog("",((SimulationView)model.getWhoPopupSimulation()).getParentFrame(),"image");			
		    			if (rep!=null) {
		    				if(!rep.endsWith("."+s)){
		    					rep+="."+s.toLowerCase();
		    				}
		    				final File file=new File(rep);
		    				if(s.equals("CSV") && getPlotFile()!=null){	    	
							
							Thread th=new Thread(new Runnable(){
								public void run() {							
									Utils.copyfile(new File(getPlotFile()),file);								
								}});
							th.start();				
						}else{	  
							
							int cbsNum = caption.getComponents().length;
							for (i = 0; i < cbsNum; i++) {
								if (((JCheckBox)caption.getComponents()[i]).isSelected()) {
									plot.addLegend(i, caption.getComponents()[i].getName());
								}
							}	
							Rectangle r=new Rectangle(1000,1000);
							BufferedImage b = plot.exportImage(plot.getFormats(),r);
							try {								
								ImageIO.write(b, s, file);
								plot.clearLegends();
								plot.repaint();							
							} catch (IOException e1) {
								// TODO Auto-generated catch block
								e1.printStackTrace();
							}
							b = null;																		
						}
					}
		    			rep=null;
		    	}
		    }
		    s=null;
		    imageFormats=null;
		    possibilities=null;
		}


		public void actionPerformed(ActionEvent e) {
			plotAction(e);
		}


		public void setPlot(Plot plot) {
			this.plot = plot;
		}			
	   }
	
	

	   
	/**
	 * @param e
	 */
	private void plotAction(ActionEvent e) {
		
		String cmd=e.getActionCommand();
		if(cmd.equals("savePlotImage")){		
			drawingArea.savePlotImage();			   
		}else if(cmd.equals("fitPlot")){
		   drawingArea.getPlot().fillPlot();
		   validate();
	   }else if(cmd.equals("customizePlot")){
		   DialogPlotCustomize customizeDialog=new DialogPlotCustomize(((SimulationView)model.getWhoPopupSimulation()).getParentFrame(),model,this);
		   customizeDialog=null;
	   }else if(cmd.equals("viewPlotData")){
		   PlotDataTable dTable = new PlotDataTable(this);
		   String nm="";
		   if(this.getName()!=null){
			   nm=this.getName();
		   }
		   model.getSimulationsPanel().tabbedPane.add(dTable, "Data"+"-"+nm);
		   model.getSimulationsPanel().tabbedPane.setSelectedIndex(model.getSimulationsPanel().tabbedPane.getTabCount()-1);
	   }else if(cmd.equals("plotExperimentalData")){
		   
		   Utils.fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		   Utils.fileChooser.setFileFilter(null);
		   String rep=Utils.showOpenDialog(((SimulationView)model.getWhoPopupSimulation()).getParentFrame(),"trace");			
		   if (rep!=null) {
			   File file=new File(rep);
			   if(!file.isDirectory()){				              	           
				   model.plotTraceData(file);
				}else{
					JOptionPane.showMessageDialog(((SimulationView)model.getWhoPopupSimulation()).getParentFrame(),"Select a file, not a whole directory!", "Selection Warning",JOptionPane.WARNING_MESSAGE);
				}
			}
	   }else if(cmd.equals("addToComparison")){
		  
		   Component[] captionComps = getCaption().getComponents();
		   JCheckBox[] cbs = new JCheckBox[captionComps.length];
		   for (int i = 0; i < captionComps.length; i++) {
			   cbs[i] = (JCheckBox) captionComps[i];
		   }
		   int cbsNum = cbs.length;
		   for (int i = 0; i < cbsNum; i++) {
			   if (cbs[i].isSelected()) {
				   getDrawingArea().getPlot().addLegend(i, cbs[i].getName());
			   }
		   }
		   captionComps = null;
		   cbs = null;
		   drawingArea.getPlot().fillPlot();					
			Rectangle r=new Rectangle(600,480);
		   BufferedImage bi2=getDrawingArea().getPlot().exportImage(getDrawingArea().getPlot().getFormats(),r);
		  // BufferedImage bi=Utils.getScaledImage(bi2,0.5);
		   ImageIcon ic =new ImageIcon(bi2);		   
		   ic.setDescription(getName());
		   BiochamDynamicTree.jplots.add(ic);
		   getDrawingArea().getPlot().clearLegends();
		   bi2=null;
		  // bi=null;
		   ic=null;
		   PlotsComparizonWindowUtils.updateComparisonWindow();
		   
	   }else if(cmd.equals("setInitFromTrace")){
		   String result = (String) JOptionPane.showInputDialog(((SimulationView)model.getWhoPopupSimulation()).getParentFrame(),
	                  "Time point",
	                  "Set Initial State from trace",
	                  JOptionPane.QUESTION_MESSAGE,
	                  null,
	                  null,
	                  0);
	         if (result != null){
	        	 try{
	        		 float value=0;
	        		 try{
	        			 value=Float.parseFloat(result);
	        		 }catch(Exception ee){
	        			 Utils.debugMsg("Init From Trace: Incorrect value...."+value+"?");
	        			 WorkbenchToolBars.infoLabel.setText("Init From Trace: Incorrect value...."+value+"?Value set to default(0).");
	        			 WorkbenchToolBars.infoLabel.setBalloonToolTipVisible(true);
	        		 }
	        		 
	        		 ((ParamTableInitConc)model.getInitConditions().getParamTable()).setDontCheck(true);
	        		 model.sendToBiocham("set_init_from_trace("+value+").\n");
	        		 
	        	 }catch(Exception ee){
	        		 
	        		 JOptionPane.showMessageDialog(((SimulationView)model.getWhoPopupSimulation()).getParentFrame(),"Incorrect time value inserted.","Warning",JOptionPane.WARNING_MESSAGE);
	        	 }
	        	 
	         }
		  
	   }
	 cmd=null;
	   
	}
	
	public void removeDataSet(String name) {
		
		int index=0;
		for(int i=0;i<dataSetsList.size();i++){
			if(dataSetsList.get(i).getLabelName().equals(name)){
				index=i;
				dataSetsList.remove(i);				
				break;		
			}
		}		
		drawingArea.getPlot().clear(index,true);
		drawingArea.getPlot().repaint();
		Component[] captionComps=caption.getComponents();
		Component[] comps=new Component[captionComps.length-1];				
	  	for(int k=0,j=0;k<captionComps.length;k++){
	  		if(!captionComps[k].getName().equals(name)){
	  			comps[j]=captionComps[k];
	  			j++;
	  		}
	  	}	
	  	caption.removeAll();
		SpringLayout sp=(SpringLayout) caption.getLayout();				
		for(int y=0;y<comps.length;y++){
			caption.add(comps[y]);			
		}
		if(comps.length>0){
			sp.putConstraint(SpringLayout.NORTH, comps[0], 35, SpringLayout.NORTH, caption);
		    sp.putConstraint(SpringLayout.WEST, comps[0], 0, SpringLayout.WEST, caption);
		    for(int k=1;k<comps.length;k++){
		    	  sp.putConstraint(SpringLayout.NORTH, comps[k], 3, SpringLayout.SOUTH, comps[k-1]);
			      sp.putConstraint(SpringLayout.WEST, comps[k], 0, SpringLayout.WEST, caption);
		    }
		}		
		caption.validate();
		caption.repaint();
		captionPane.validate();
		captionPane.repaint();
		validate();		
	}

	public void addDataSet(DataSet ds) {		
				
		dataSetsList.add(ds);
		addDataSetToPlot(ds);
		JCheckBox cb=new JCheckBox(ds.getLabelName());
		caption.add(cb);
		cb.setName(ds.getLabelName());
   	  	cb.setActionCommand(cb.getName());
   	  	cb.setBackground(Color.WHITE);
   	  	cb.setFont(new Font("",Font.PLAIN,10));
   	  	cb.setSelected(true);
   	  	cb.setForeground(ds.getColor());
   	  	cb.addItemListener(caption);
   	  	cb.addMouseListener(new MouseAdapter(){
   		  
   	  		public void mouseClicked(MouseEvent e) {
        		
   	  			if(e.getSource() instanceof JCheckBox){
   	  				JCheckBox cb=(JCheckBox)e.getSource();
   	  				if(e.getButton() == MouseEvent.BUTTON3){
   	  					String dataset=cb.getName();
   	  					int i=getDatasetIndex(dataset);					               
   	  					caption.setColor(i, cc.showDialog(((SimulationView)model.getWhoPopupSimulation()).getParentFrame(),"Choose a color",dataSetsList.get(i).getColor()));
   	  					cb.setForeground(dataSetsList.get(i).getColor());					                
   	  				}
   	  			}
   	  		} 		  
   	  });
   	  caption.add(cb);
   	  SpringLayout sp=(SpringLayout) caption.getLayout();
   	  Component[] cbs=caption.getComponents();
   	  caption.removeAll();
   	  for(int i=0;i<cbs.length;i++){
    	 caption.add(cbs[i]);
   	  }
     sp.putConstraint(SpringLayout.NORTH, caption.getComponent(0), 35, SpringLayout.NORTH, caption);
     sp.putConstraint(SpringLayout.WEST, caption.getComponent(0), 0, SpringLayout.WEST, caption);
     for(int i=1;i<caption.getComponentCount();i++){
    	 sp.putConstraint(SpringLayout.NORTH, caption.getComponent(i), 3, SpringLayout.SOUTH, caption.getComponent(i-1));
    	 sp.putConstraint(SpringLayout.WEST, caption.getComponent(i), 0, SpringLayout.WEST, caption);
     }   
     int max=30,temp=0;
     for(int i=0;i<caption.getComponentCount();i++){
    	 if(caption.getComponent(i) instanceof JCheckBox){
    		 temp=((JCheckBox)caption.getComponent(i)).getName().length();
    		 if(temp>=max){
    			 max=temp;
    		 }
   	 	 }
     }
     cbs=null;
     sp=null;
     caption.setPreferredSize(new Dimension(max*7,caption.getComponentCount()*30));
     caption.revalidate();
     captionPane.revalidate();		
	}

	private void addDataSetToPlot(DataSet ds) {
		
		
		int index=getDatasetIndex(ds.getLabelName());
		if(ds.isErrorBars()){
			ArrayList<String> values=ds.getValuesInPairs();
			for(int i=0;i<values.size();i++){
				StringTokenizer st=new StringTokenizer(values.get(i),",");
				ArrayList<Double> args=new ArrayList<Double>();
				while(st.hasMoreTokens()){
					String s=st.nextToken();
					args.add(Double.parseDouble(s));
				}
				drawingArea.getPlot().setConnected(ds.isConnected(),index);
				drawingArea.getPlot().addPointWithErrorBars(index, args.get(0), args.get(1), args.get(2), args.get(3), ds.isConnected());
				drawingArea.getPlot().setColor(index,ds.getColor());		
				args.clear();
				args=null;
			}
		}else{
			ArrayList<String> values=ds.getValuesInPairs();
			for(int i=0;i<values.size();i++){
				StringTokenizer st=new StringTokenizer(values.get(i),",");
				ArrayList<Double> args=new ArrayList<Double>();
				while(st.hasMoreTokens()){
					String s=st.nextToken();
					args.add(Double.parseDouble(s));
				}
				drawingArea.getPlot().setConnected(ds.isConnected(),index);
				drawingArea.getPlot().addPoint(index, args.get(0), args.get(1), ds.isConnected());
				drawingArea.getPlot().setColor(index,ds.getColor());
				drawingArea.getPlot().setMarksStyle(ds.getMarks(), index);
				args.clear();
				args=null;
			}
		}
		drawingArea.getPlot().repaint();
	}

	public void applyPlotProperties(PlotProperties pp) {
	
		if(!pp.getAutorange()){
			if(pp.getXRange()!=null){
				String s=pp.getXRange();
				StringTokenizer st=new StringTokenizer(s,",");
				Double r1=0.0,r2=0.0;
				if(st.countTokens()==2){
					r1=Double.valueOf(st.nextToken());
					r2=Double.valueOf(st.nextToken());
				}
				drawingArea.getPlot().setXRange(r1,r2);
			}
			if(pp.getYRange()!=null){
				String s=pp.getYRange();
				StringTokenizer st=new StringTokenizer(s,",");
				Double r1=0.0,r2=0.0;
				if(st.countTokens()==2){
					r1=Double.valueOf(st.nextToken());
					r2=Double.valueOf(st.nextToken());
				}
				drawingArea.getPlot().setYRange(r1,r2);
			}
		}
		if(pp.getBars()){
			drawingArea.getPlot().setBars(true);
		}else{
			drawingArea.getPlot().setBars(false);
		}
		if(pp.getGrid()){
			drawingArea.getPlot().setGrid(true);
		}else{
			drawingArea.getPlot().setGrid(false);
		}
		if(pp.getImpulses()){
			drawingArea.getPlot().setImpulses(true);
		}else{
			drawingArea.getPlot().setImpulses(false);
		}
		if(pp.getLines()){
			drawingArea.getPlot().setConnected(true);
		}else{
			drawingArea.getPlot().setConnected(false);
		}
		if(pp.getXLogAxis()){
			drawingArea.getPlot().setXLog(true);
		}else{
			drawingArea.getPlot().setXLog(false);
		}
		if(pp.getYLogAxis()){
			drawingArea.getPlot().setYLog(true);
		}else{
			drawingArea.getPlot().setYLog(false);
		}		
		if(pp.getMarks()!=null){
			drawingArea.getPlot().setMarksStyle(pp.getMarks());
		}
		if(pp.getPlotTitle()!=null){
			drawingArea.getPlot().setTitle(pp.getPlotTitle());
		}
		if(pp.getXLabel()!=null){
			drawingArea.getPlot().setXLabel(pp.getXLabel());
		}
		if(pp.getYLabel()!=null){
			drawingArea.getPlot().setYLabel(pp.getYLabel());
		}			
		drawingArea.getPlot().repaint();
		drawingArea.repaint();
	}
	
	
	
	public void applyDataSetProperties(DatasetProperties pp) {
		
		String name=pp.getDatasetName();
		int index=getDatasetIndex(name);
		DataSet ds=dataSetsList.get(index);
		
		if(pp.getMarks()!=null){
			drawingArea.getPlot().setMarksStyle(pp.getMarks(), index);
			ds.setMarks(pp.getMarks());
		}	
		if(pp.getImpulses()){
			drawingArea.getPlot().setImpulses(true, index);			
		}else{
			drawingArea.getPlot().setImpulses(false, index);
		}
		if(pp.getBars()){
			drawingArea.getPlot().setBars(true);
		}else{
			drawingArea.getPlot().setBars(false);
		}
		if(pp.getColor()!=null){
			Color c1=pp.getColor();
			Color current=dataSetsList.get(index).getColor();
			if(c1!=current){				
				for(int i=0;i<dataSetsList.size();i++){
					if(i==index){
						drawingArea.getPlot().setColor(i,c1);
						drawingArea.getPlot().repaint();
						dataSetsList.get(i).setColor(c1);
						ds.setColor(c1);
						Component[] captionComps=caption.getComponents();					      
					    for(int j=0;j<captionComps.length;j++){
					    	if(captionComps[j].getName().equals(name)){
					    		captionComps[j].setForeground(c1);
					    		break;
					    	}					    	
					    }
					    break;
					}
				}					
			}
		}
		if(ds.isConnected()!=pp.getLines()){
			ds.setConnected(pp.getLines());
			drawingArea.getPlot().clear(index,false);
			addDataSetToPlot(ds);			
		}	
		caption.repaint();
		drawingArea.repaint();
		validate();
	}

	
	
	
	
	
	public UUID getIndex() {
		return index;
	}
	public void setIndex(UUID index) {
		this.index = index;
	}
	public String getPlotFile() {
		return plotFile;
	}
	public void setPlotFile(String plotFile) {
		this.plotFile = plotFile;
	}
	public PlotCaption getCaption() {
		return caption;
	}
	public void setCaption(PlotCaption caption) {
		this.caption = caption;
	}
	public PlotDataCSV getPlotData() {
		return plotData;
	}
	public void setPlotData(PlotDataCSV plotData) {
		this.plotData = plotData;
	}
	public BiochamModel getModel() {
		return model;
	}
	public void setModel(BiochamModel model) {
		this.model = model;
	}
	private void addYTick(String string, double d) {
		drawingArea.getPlot().addYTick(string,d);
		drawingArea.getPlot().repaint();
	}
	private void addXTick(String string, double d) {
		drawingArea.getPlot().addXTick(string,d);
		drawingArea.getPlot().repaint();
	}	
	private void setTitle(String string) {
		drawingArea.getPlot().setTitle(string);
		drawingArea.getPlot().repaint();		
	}
	private void setAutoRange(boolean b) {}
	
	private void setGrid(boolean b) {
		drawingArea.getPlot().setGrid(b);
		drawingArea.getPlot().repaint();		
	}	
	private void resetSize() {
		drawingArea.getPlot().fillPlot();		
	}	
	
	 /** Zoom in or out to the specified rectangle.
     *  This method calls repaint().
     *  @param lowx The low end of the new X range.
     *  @param lowy The low end of the new Y range.
     *  @param highx The high end of the new X range.
     *  @param highy The high end of the new Y range.
     */
	public void zoomIn() {
		drawingArea.getPlot().zoom(drawingArea.getPlot().getPlotRectangle().getMinX()+0.1, drawingArea.getPlot().getPlotRectangle().getMinY()+0.1, drawingArea.getPlot().getPlotRectangle().getMaxX()+0.1,drawingArea.getPlot().getPlotRectangle().getMaxY()+0.1);
		
	}	
	
	public void zoomOut() {
		drawingArea.getPlot().zoom(drawingArea.getPlot().getPlotRectangle().getMinX()-0.1, drawingArea.getPlot().getPlotRectangle().getMinY()-0.1, drawingArea.getPlot().getPlotRectangle().getMaxX()-0.1,drawingArea.getPlot().getPlotRectangle().getMaxY()-0.1);
		
	}	
	
	public ArrayList<DataSet> getDataSetsList() {
		return dataSetsList;
	}	
	public int getDatasetIndex(String dataset) {							
		int index=0;
		if(dataSetsList!=null){
			for(int i=0;i<dataSetsList.size();i++){
				if(dataSetsList.get(i).getLabelName().equals(dataset)){
					index=i;
				}
			}	
		}
		return index;
   }
	public DataSet getDataSetByName(String name) {
		DataSet ds=null;
		for(int i=0;i<dataSetsList.size();i++){
			ds=dataSetsList.get(i);
			if(ds.getLabelName().equals(name)){
				return ds;
			}
		}		
		return ds;
	}	
	public void setDataSetsList(ArrayList<DataSet> dataSetsList) {
		this.dataSetsList = dataSetsList;
	}	
	public PlotCanvas getDrawingArea() {
		return drawingArea;
	}	
	public void setDrawingArea(PlotCanvas drawingArea) {
		this.drawingArea = drawingArea;
	}
	public void actionPerformed(ActionEvent e) {
	   plotAction(e);	   
   }	
	public void disposePlot(){
		
		drawingArea=null;
		caption=null;
		model=null;
		name=null;	   
	    cc=null;	 
		captionPane=null;
		if(dataSetsList!=null){
			dataSetsList.clear();
		}	
		dataSetsList=null;
		index=null;
		plotFile=null;
		if(plotData!=null){
			plotData.disposeData();
		}		
		plotData=null;		   
	}

	public boolean isWithoutMistakes() {
		return withoutMistakes;
	}

	public void setWithoutMistakes(boolean withoutMistakes) {
		this.withoutMistakes = withoutMistakes;
	}		      
}
