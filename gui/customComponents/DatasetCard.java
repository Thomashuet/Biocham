package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.plotting.DataSet;
import fr.inria.contraintes.biocham.plotting.DatasetProperties;
import fr.inria.contraintes.biocham.plotting.DatasetPropertiesPanel;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.SwingWorker;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.StringTokenizer;

import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;



/**
 * Class thats creates a custom JPanel component for previewing and modifying the datasets plotted on the simulation result.
 * 
 * Used by the DialogPlotCustomize dialog.
 * 
 * @author Dragana Jovanovska  
 */ 
public class DatasetCard extends JPanel implements ActionListener{
		
	
	DefaultListModel listModel;
	JList datasetList;
	BiochamPlot biochamPlot;
	JPanel cards,properties;
	BiochamModel model;
	DatasetPropertiesPanel datasetPanel;
	JButton addNew,remove;
	DatasetProperties datasetProperties;
	JPanel buttonsPanel;
	JPanel applyButtons;
	JDialog parent;
	
	
	
	public DatasetCard(BiochamPlot bp,JPanel ab,JDialog _parent){
		
		super();	
		boolean b=SwingUtilities.isEventDispatchThread();		
		parent=_parent;
		applyButtons=ab;
		biochamPlot=bp;	
		model=bp.getModel();		
		
		setLayout(new BorderLayout());
		initialize();			
		if(listModel.getSize()==0){
			removeAll();
			JPanel p=datasetPanel.getNewDSPanel();			
			addNew.setText("Add");
			addNew.setActionCommand("addDataSet");
			remove.setText("Cancel");
			remove.setActionCommand("cancelAddingDataset");			
			add(p,BorderLayout.CENTER);
			add(buttonsPanel,BorderLayout.SOUTH);
			applyButtons.setVisible(false);
			revalidate();
			repaint();
		}		
	}




	/**
	 * 
	 */
	private void initialize() {
		listModel = new DefaultListModel();
		listModel.addListDataListener(new ListDataListener(){
			 public void contentsChanged(ListDataEvent e) {}
			 public void intervalAdded(ListDataEvent e) {}
			 public void intervalRemoved(ListDataEvent e) {
				
				 if(listModel.getSize()==0){
					 removeAll();
					 JPanel p=datasetPanel.getNewDSPanel();			
					 addNew.setText("Add");
					 addNew.setActionCommand("addDataSet");
					 remove.setText("Cancel");
					 remove.setActionCommand("cancelAddingDataset");
					 add(p,BorderLayout.CENTER);
					 add(buttonsPanel,BorderLayout.SOUTH);		
					 applyButtons.setVisible(false);
					 revalidate();
					 repaint();
				 }
			 }
		});
		for(int i=0;i<biochamPlot.getDataSetsList().size();i++){
			listModel.add(i,biochamPlot.getDataSetsList().get(i).toString());
		}		
		datasetList=new JList(listModel);
		datasetList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		datasetList.setVisibleRowCount(8);		
		datasetList.setCellRenderer(new CustomCellRenderer());
		datasetList.addListSelectionListener(new ListSelectionListener(){
			public void valueChanged(ListSelectionEvent e) {
				JList list=(JList)e.getSource();
				int sel=list.getSelectedIndex();
				if(sel>=0){
					String name=((JList)e.getSource()).getSelectedValue().toString();
					DataSet ds=biochamPlot.getDataSetByName(name);
					cards.add(datasetPanel.getDSPanel(ds),BorderLayout.CENTER);	
				}else{
					if(listModel.getSize()-1>0){
						datasetList.setSelectedIndex(listModel.getSize()-1);
					}
				}			
			}});
		UniversalScrollPane sp=new UniversalScrollPane(datasetList);
		sp.setBorder(null);
		sp.setBackground(Color.white);		
		add(sp,BorderLayout.WEST);				
		properties=new JPanel(new BorderLayout());		
		buttonsPanel=new JPanel();
		addNew=new JButton("Add");
		addNew.addActionListener(this);
		addNew.setActionCommand("addNewDataset");
		remove=new JButton("Remove");
		remove.addActionListener(this);
		remove.setActionCommand("removeDataset");
		buttonsPanel.add(addNew);
		buttonsPanel.add(remove);
		properties.add(buttonsPanel,BorderLayout.SOUTH);				
		cards=new JPanel(new BorderLayout());
		datasetPanel=new DatasetPropertiesPanel(model);
		cards.add(datasetPanel.getDSPanel(),BorderLayout.CENTER);		
		properties.add(cards,BorderLayout.CENTER);
		properties.setPreferredSize(new Dimension(400,350));
		UniversalScrollPane sp1=new UniversalScrollPane(properties);
		sp1.setSize(properties.getPreferredSize());
		add(sp1,BorderLayout.CENTER);
		datasetList.setSelectedIndex(0);			
		revalidate();
		repaint();
		
	}

	
		
	
	public void actionPerformed(ActionEvent e) {
		
		String cmd=e.getActionCommand();
		if(cmd.equals("addNewDataset")){
			
			JPanel p=datasetPanel.getNewDSPanel();			
			addNew.setText("Add");
			addNew.setActionCommand("addDataSet");
			remove.setText("Cancel");
			remove.setActionCommand("cancelAddingDataset");			
			cards.add(p,BorderLayout.CENTER);			
			applyButtons.setVisible(false);
			
		}else if(cmd.equals("removeDataset")){
			
			String name=datasetList.getSelectedValue().toString();
			int index=datasetList.getSelectedIndex();
			JCheckBox cb=biochamPlot.getCaption().getCheckBoxByName(name);
			if(cb!=null){
				cb.setVisible(false);
			}
			if(listModel.getSize()-1>0){
				datasetList.setSelectedIndex(0);
			}
			listModel.remove(index);
			listModel.trimToSize();
			datasetList.validate();			
			biochamPlot.removeDataSet(name);
			
		}else if(cmd.equals("addDataSet")){
					
			
			SwingWorker sw=new SwingWorker(){
				DataSet ds;
				@Override
				public Object construct() {
					datasetProperties=datasetPanel.getDatasetProperties(true);
					if(datasetProperties==null){
						return null;
					}else{
						ds=createDataSetFromProperties(datasetProperties);
						return ds;
					}
				}

				@Override
				public void finished() {
					if(datasetProperties!=null){
						biochamPlot.addDataSet(ds);		
						applyButtons.setVisible(true);
						if(biochamPlot.getDataSetsList().size()==1){
							removeAll();
							initialize();	
						}else{
							listModel.addElement(ds);
							datasetList.setSelectedIndex(0);
							int size=listModel.getSize();
							JPanel p=datasetPanel.getDSPanel(biochamPlot.getDataSetByName(ds.getLabelName()));
							addNew.setText("Add");
							addNew.setActionCommand("addNewDataset");	
							remove.setText("Remove");
							remove.setActionCommand("removeDataset");
							cards.add(p,BorderLayout.CENTER);
						}
					}
				}};
				sw.start();
			
			
		}else if(cmd.equals("cancelAddingDataset")){
			
			applyButtons.setVisible(true);
			if(biochamPlot.getDataSetsList().size()==1){
				removeAll();
				initialize();	
			}else{			
				
				datasetList.setSelectedIndex(0);
				String name=listModel.get(0).toString();
				JPanel p=datasetPanel.getDSPanel(biochamPlot.getDataSetByName(name));
				addNew.setText("Add");
				addNew.setActionCommand("addNewDataset");			
				remove.setText("Remove");				
				remove.setActionCommand("removeDataset");
				cards.add(p,BorderLayout.CENTER);
				buttonsPanel.revalidate();
				revalidate();
				repaint();
			}
		}		
	}
	
	
	private DataSet createDataSetFromProperties(DatasetProperties dsp) {
		
		
		boolean b=SwingUtilities.isEventDispatchThread();
		
		
		DataSet ds=new DataSet();
		ds.setColor(dsp.getColor());
		ds.setLabelName(dsp.getDatasetName());
		ds.setConnected(dsp.getLines());
		if(dsp.getMarks().equals("errorBars")){
			ds.setErrorBars(true);
		}else{
			ds.setMarks(dsp.getMarks());
		}
		//1,2,3;4,5,6;7,8,9; //or, 1,2;3,4;
		ArrayList<String> vals=dsp.getValues();
		ArrayList<String> xVals=new ArrayList<String>();
		ArrayList<String> yVals=new ArrayList<String>();
		ArrayList<String> zVals=new ArrayList<String>();
		ArrayList<String> z1Vals = null,z2Vals = null;
		
		for(int i=0;i<vals.size();i++){
			StringTokenizer st=new StringTokenizer(vals.get(i),",");
			// 1,2,3   6,7,8    5,8,9   7,4,3   x,y,z
			
			if(st.countTokens()==2){
				while(st.hasMoreTokens()){
					xVals.add(st.nextToken());
					yVals.add(st.nextToken());
				}
			}else if(st.countTokens()==3){
				ds.setErrorBars(true);
				while(st.hasMoreTokens()){
					xVals.add(st.nextToken());
					yVals.add(st.nextToken());
					zVals.add(st.nextToken());					
				}
			}
		}
		if(ds.isErrorBars()){
			z1Vals=new ArrayList<String>();
			z2Vals=new ArrayList<String>();
			for(int i=0;i<zVals.size();i++){
				double sdL=Double.parseDouble(yVals.get(i)) - Double.parseDouble(zVals.get(i));
				double sdH=Double.parseDouble(yVals.get(i)) + Double.parseDouble(zVals.get(i));
				z1Vals.add(String.valueOf(sdL));
				z2Vals.add(String.valueOf(sdH));
				
			}
		}
		ArrayList<String> valuesInPairs=new ArrayList<String>();		
		for(int i=0;i<xVals.size();i++){
			if(ds.isErrorBars()){
				valuesInPairs.add(xVals.get(i)+","+yVals.get(i)+","+z1Vals.get(i)+","+z2Vals.get(i));
			}else{
				valuesInPairs.add(xVals.get(i)+","+yVals.get(i));
			}
		}
		ds.setYValues(yVals);
		ds.setValuesInPairs(valuesInPairs);
		ds.setXTicks(xVals);
		return ds;
	}


	class CustomCellRenderer extends JLabel implements ListCellRenderer {

		
		public Component getListCellRendererComponent(JList list, Object value,
				int index, boolean isSelected, boolean cellHasFocus) {
			
			  String s = value.toString();			  
			  int ind=biochamPlot.getDatasetIndex(s);
			  Color dsColor=biochamPlot.getDataSetsList().get(ind).getColor();
		         setText(s);		         
		         setIcon(Icons.icons.get("metalcomplex.gif"+0.1));
		           if (isSelected) {
		             setBackground(list.getSelectionBackground());
		             setForeground(dsColor.darker());
		           }
		         else {
		              setBackground(list.getBackground());
		              setForeground(dsColor);
		           }
		           setEnabled(list.isEnabled());
		           setFont(list.getFont());
		         setOpaque(true);
		         return this;
		}

	}
	
	
	
	
	public DatasetPropertiesPanel getDatasetPanel() {
		return datasetPanel;
	}
	public void setDatasetPanel(DatasetPropertiesPanel datasetPanel) {
		this.datasetPanel = datasetPanel;
	}

	
}
