package fr.inria.contraintes.biocham.graphicalEditor;

import fr.inria.contraintes.biocham.customComponents.CustomMultiSelectComboBox;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.util.ArrayList;

import javax.swing.SwingUtilities;



public class GraphElementsComboBox{


	CustomMultiSelectComboBox comboBox;
	CustomIconedCellRenderer renderer;
	ArrayList<String> selected;
	ArrayList<String> allItems;
	boolean finished=false;
	
	
	
	
	public GraphElementsComboBox(String[] s) {
		 
		
		boolean b=SwingUtilities.isEventDispatchThread();
		
		
		selected=new ArrayList<String>();
		allItems=new ArrayList<String>();
		
		comboBox = new CustomMultiSelectComboBox(this);
		
		comboBox.getModel().setSelectedItem("");     
		comboBox.addItem("Select All");
        for(int i = 0; i < s.length; i++) {
        	comboBox.addItem(s[i]);
        	allItems.add(s[i]);
         }
        
        Dimension d = comboBox.getPreferredSize();
        comboBox.setPreferredSize(new Dimension(130, d.height));
        comboBox.setPopupWidth(d.width+20);	  
       
        renderer = CustomIconedCellRenderer.getCheckBoxRendererInstance();
        comboBox.setRenderer( renderer );
        comboBox.addActionListener(new ActionListener(){
        
        	public void actionPerformed(ActionEvent e){
        		
        		int index=comboBox.getSelectedIndex();
        		boolean isSelected = renderer.getSelStateList()[index];
        		if(comboBox.getSelectedItem().equals("Select All")){
        			if(isSelected){
        				selected.clear();
        				for(int i=0;i<comboBox.getItemCount();i++){
        					if(comboBox.getItemAt(i).equals("Source/Sink")){
        						renderer.setEnabled(i,false);
        					}else{        						
        						if(!comboBox.getItemAt(i).equals("Select All")){
        							selected.add(comboBox.getItemAt(i).toString());
        						}        						
        						renderer.getSelStateList()[i]=true;
        					}
        				}
        			}else{
        				selected.clear();
        				for(int i=0;i<comboBox.getItemCount();i++){
        					if(comboBox.getItemAt(i).equals("Source/Sink")){
        						renderer.setEnabled(i,true);
        					}    						
    						renderer.getSelStateList()[i]=false;
        				}
        				comboBox.getModel().setSelectedItem("");     
        			}
        		}else if(comboBox.getSelectedItem().equals("Source/Sink")){
        			if(isSelected){
        				selected.clear();
        				for(int i=0;i<comboBox.getItemCount();i++){
        					if(comboBox.getItemAt(i).equals("Source/Sink")){
        						renderer.setEnabled(i,true);
        					}else{        						
        						renderer.setEnabled(i,false);
        					}  
        				}
        				selected.add(comboBox.getSelectedItem().toString());
        			}else{
        				selected.clear();
        				for(int i=0;i<comboBox.getItemCount();i++){
        					renderer.setEnabled(i,true);        					
        				}
        				comboBox.getModel().setSelectedItem("");     
        			}
        		}
        	}
        });
        comboBox.addKeyListener(new KeyAdapter(){
        	public void keyPressed(KeyEvent e){
        		if(e.getSource() instanceof CustomMultiSelectComboBox){
        			if(e.getKeyCode()==KeyEvent.VK_ENTER){        				
        				CustomMultiSelectComboBox mcb=(CustomMultiSelectComboBox)e.getSource();
        				mcb.setPopupVisible(false);        				
        			}
        		}
        	}
        });
	}
	
	public CustomMultiSelectComboBox getMultiSelectComboBox(){
		return comboBox;
	}
	public ArrayList<String> getSelected() {
		return selected;
	}
	public ArrayList getAllItems() {		
		return allItems;
	}
	public CustomIconedCellRenderer getRenderer() {
		return renderer;
	}
	public boolean isFinished() {
		return finished;
	}
	public void setFinished(boolean finished) {
		this.finished = finished;
	}
	
	
}
