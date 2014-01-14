package fr.inria.contraintes.biocham.customComponents;

import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.DrawingSupplier;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYStepRenderer;
import org.jfree.data.Range;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;
import org.jfree.ui.RectangleInsets;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.modelData.SimulationView;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.geom.Rectangle2D;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.StringTokenizer;
import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.modelData.SimulationView;
import javax.swing.JOptionPane;
import javax.swing.JPanel;




/**
 * Class thats creates the the plotting area for visualizing the numerical simulation results done by the biocham boolean simulator.
 * 
 * @author Dragana Jovanovska  
 */ 
public class BooleanPlot  extends JPanel{// implements ActionListener{

	
	XYSeriesCollection xyDatasets;
	HashMap<String,ArrayList<Integer>> datasets;
	ArrayList<String> chosenTitles,allTitles;
	JFreeChart chart;
	int transitions=0;
	String plotName;
	String plotFile;
	
	public BooleanPlot(String file,String modelName,String pn){
		
		super(new BorderLayout());
		plotName=pn;
		try {
			
			parseFile(file);
			createPlot(modelName);
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
		
	private void createPlot(String title) {
		
		
		NumberAxis ay=new NumberAxis(" ");
	    ay.setAutoRange(false);
	    Range ry=new Range(0,chosenTitles.size()*2);
	    ay.setRange(ry);
	    ay.setMinorTickMarksVisible(false);
	    ay.setTickLabelsVisible(false);
	    ay.setAutoTickUnitSelection(false);
	    ay.setTickMarkStroke(new BasicStroke(2.0f));	    
	    ay.setTickMarkOutsideLength(3);
	    ay.setTickMarkPaint(Color.BLACK);
	    
	    NumberAxis ax=new NumberAxis("Transitions");
	    ax.setAutoRange(false);
	    Range rx=new Range(0,transitions-1);
	    ax.setRange(rx);
	    ax.setMinorTickMarksVisible(false);
	    ax.setTickLabelsVisible(true);
	    ax.setAutoTickUnitSelection(false);
	    ax.setTickMarkStroke(new BasicStroke(2.0f));
	    ax.setTickMarkOutsideLength(3);
	    ax.setTickMarkPaint(Color.BLACK);
	    
	    xyDatasets=new XYSeriesCollection();
	    XYSeries data=null;
	    for(int i=0;i<chosenTitles.size();i++){
	    	
	    	String chosen=chosenTitles.get(i);
	    	data = new XYSeries(chosenTitles.get(i),true,true);
	    	for(int j=0;j<transitions;j++){
	    		data.add(j,datasets.get(chosenTitles.get(i)).get(j));
	    	}
	    	xyDatasets.addSeries(data);
	    }
	    data=null;
	    XYPlot plot = new XYPlot(xyDatasets,ax, ay,new XYStepRenderer());
	    plot.setBackgroundPaint(Color.WHITE);
	    plot.setDomainGridlinePaint(Color.WHITE);
	    plot.setRangeGridlinePaint(Color.WHITE);
	    plot.setAxisOffset(new RectangleInsets(5.0, 5.0, 5.0, 5.0));
	    plot.setDomainZeroBaselineVisible(true);
	    plot.setRangeZeroBaselineVisible(true);
	    plot.setRangeGridlinesVisible(false);
	    plot.setDomainGridlinesVisible(false);
	    plot.setBackgroundPaint(Color.WHITE);
        plot.getRenderer().setSeriesStroke(0, new BasicStroke(2.0f));
        plot.getRenderer().setSeriesStroke(1, new BasicStroke(2.0f));
       
        
        chart=new JFreeChart(plot);      
	    chart.setAntiAlias(false);
	    chart.setTextAntiAlias(false);
	    chart.setTitle(title);
	    chart.setBackgroundPaint(Color.WHITE);
	    
        
	   
	    CustomChartPanel chartPanel = new CustomChartPanel(this);//chart,getPlotName());
        chartPanel.setBackground(Color.WHITE);
        chartPanel.setMouseZoomable(true);     
   
        chartPanel.setDisplayToolTips(true);
        chartPanel.setPreferredSize(new Dimension(1000,1000));
        chartPanel.setMaximumDrawHeight(700);
        chartPanel.setMinimumDrawWidth(700);
        
        add(chartPanel,BorderLayout.CENTER);
        
	((SimulationView)BiochamDynamicTree.currentModel.getWhoPopupSimulation()).setProgressBarDone();

	}



	private void parseFile(String f) throws IOException {
		
		
		BufferedReader in = new BufferedReader(new FileReader(f.trim()));			
		
		datasets=new HashMap<String,ArrayList<Integer>>();
		
		ArrayList<String> dataSets = null;
		String temp=null;			
		int start=0;
		String s = null;	
		while ((s = in.readLine()) != null) {	
			
			if (s.startsWith("plot")) {					
				dataSets=new ArrayList<String>();
				String dsets=s.substring(5);	
				while(dsets.contains(", ")){
					int op=dsets.indexOf(", ");
					String dss=dsets.substring(start,op);					
					dataSets.add(dss);
					dsets=dsets.substring(op+2);
					dss=null;					
				}				
				dataSets.add(dsets);			
			}		
		}	
		s=null;
		in.close();
		in=null;
		
		String cvsFileName=null;//=new ArrayList<String>();
		
		if(dataSets!=null){
			
			chosenTitles=new ArrayList<String>(dataSets.size());
			
			int size1=dataSets.size();
			StringTokenizer st;
			for(int i=0;i<dataSets.size();i++){
				
				//String datasetNm=dataSets.get(i);
				st=new StringTokenizer(dataSets.get(i));
				while(st.hasMoreTokens()){
					temp=st.nextToken();					
					if(temp.contains(".csv")){
						String tempy=temp.substring(1,temp.length()-1);
						cvsFileName=tempy;
						tempy=null;
						//break;
					}
					else if(temp.contains(":") && !temp.contains("::")){
					
					}else if(temp.equals("title")){
						temp=st.nextToken();
						String nm=temp.substring(1,temp.length()-1);
						chosenTitles.add(nm);
						nm=null;
					}					
				}								
			}
			st=null;
			int size2=chosenTitles.size();			
			parseCSVFile(cvsFileName);			
			cvsFileName=null;
		}
		dataSets.clear();
		dataSets = null;
		temp=null;		
		s = null;			
			
		
	}



	private void parseCSVFile(String f) {
		
		BufferedReader br=null;
		allTitles=new ArrayList<String>();
		boolean error=false;
		setPlotFile(f);
		try {
			
			br = new BufferedReader(new FileReader(f));
			String line="";
			boolean firstRow=false;
			StringTokenizer st;
			while((line=br.readLine())!=null){
				 
				try{
					
					int i=0;					
					st=new StringTokenizer(line);				
					
					if(!firstRow){	
					
						String s;
						while(st.hasMoreTokens()){
							
							s=st.nextToken();
							if(s.startsWith("#")){
								s=s.substring(1,s.length());
							}
							datasets.put(s,new ArrayList<Integer>());	
							allTitles.add(s);
						}
						firstRow=true; 
						s=null;
					}else{
					
						while(st.hasMoreTokens()){
											
							datasets.get(allTitles.get(i)).add(Integer.parseInt(st.nextToken()));														
							i++;						
						}										
					}  
								
				}catch(Exception e){
					
					error=true;
					br.close();
					e.printStackTrace();
					st=null;
					break;					
				}
			}	
			st=null;
			line=null;			
			transitions=datasets.get(allTitles.get(0)).size();			
			br.close();
			br=null;
			
		} catch (FileNotFoundException e) {
			br=null;
			JOptionPane.showMessageDialog(((SimulationView)BiochamDynamicTree.currentModel.getWhoPopupSimulation()).getParentFrame(), "The input(plot file) is incorrect","Warning",JOptionPane.WARNING_MESSAGE);
			e.printStackTrace();
		} catch (IOException e) {
			br=null;
			JOptionPane.showMessageDialog(((SimulationView)BiochamDynamicTree.currentModel.getWhoPopupSimulation()).getParentFrame(), "Reading input file error.","Warning",JOptionPane.WARNING_MESSAGE);
			e.printStackTrace();
		}
		if(error){
			br=null;
			JOptionPane.showMessageDialog(((SimulationView)BiochamDynamicTree.currentModel.getWhoPopupSimulation()).getParentFrame(), "Reading input file error.","Warning",JOptionPane.WARNING_MESSAGE);			
		}
		
	}

	public String getPlotName() {
		return plotName;
	}

	public void setPlotName(String plotName) {
		this.plotName = plotName;
	}
	
	public String getPlotFile() {
		return plotFile;
	}
	public void setPlotFile(String plotFile) {
		this.plotFile = plotFile;
	}
}
