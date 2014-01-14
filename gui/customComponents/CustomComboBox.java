package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.graphicalEditor.CustomIconedCellRenderer;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.SwingUtilities;


/**
 * Class thats creates a custom combobox, with a possibility for multi selection(CustomMultiSelectComboBox.java) and a renderer for a custom preview of the listed items.
 * 
 * Used in the DialogSearchSarams dialog, the DialogNewMolecule dialog, the CmaesConditionPanel and in the ReactionDialog dialog.
 * 
 * @author Dragana Jovanovska  
 */ 
public class CustomComboBox{
	

	CustomMultiSelectComboBox comboBox;
	CustomIconedCellRenderer renderer;
	ArrayList<String> selected;
	ArrayList<String> allItems;
	boolean finished=false;
	boolean graphEditor=false;
	
	
	
	public CustomComboBox(String[] s, boolean graphEditor) {
		 
		
		boolean b=SwingUtilities.isEventDispatchThread();
		this.graphEditor=graphEditor;
		final int sourceSink=this.graphEditor?1:0;
		
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
        	        	
        		if(!finished && renderer!=null && renderer.getSelStateList()!=null && comboBox.getSelectedIndex()!=-1){
	        		
	        		int index=comboBox.getSelectedIndex();
	        		boolean isSelected = renderer.getSelStateList()[index];
	        		if(isSelected){
	        			
	        			//deselected	        			
	        			if(comboBox.getSelectedItem().equals("Select All") && renderer.getEnableState()[index]){
	        			
	        				for(int i=0;i<renderer.getSelStateList().length;i++){
	        					if(comboBox!=null && comboBox.getItemAt(i)!=null && !comboBox.getItemAt(i).toString().contains("Source/Sink")){
	        						renderer.getSelStateList()[i]=false;
	        					}
	        				}
	        			}else if(comboBox.getSelectedItem().equals("Source/Sink")){
	        				
	        				int len=renderer.getSelStateList().length;
	        				for(int i=0;i<len;i++){
	        					if(!comboBox.getItemAt(i).toString().contains("Source/Sink")){
	        						renderer.getEnableState()[i]=true;
	        					}else{
	        						renderer.getSelStateList()[i]=false;
	        					}	        					
	        				}
	        				
	        			}
	        			selected.remove(comboBox.getSelectedItem());
	        			renderer.getSelStateList()[index]=false;
	        			comboBox.getModel().setSelectedItem("");   
	        			
	        		}else{
	        			//selected
	        			if(comboBox.getSelectedItem().equals("Select All") && renderer.getEnableState()[index]){
	        			
	        				int len=comboBox.getItemCount();;//renderer.getSelStateList().length;
	        				int len2=renderer.getSelStateList().length;
	        				for(int i=0;i<len;i++){
	        					if(sourceSink==1){
	        						if(comboBox!=null && comboBox.getItemAt(i)!=null && comboBox.getItemAt(i).toString().contains("Source/Sink")){
		        						renderer.getSelStateList()[i]=false;
		        					}else{
		        						renderer.getSelStateList()[i]=true;
		        					}	
	        					}else{
	        						renderer.getSelStateList()[i]=true;
	        					}
	        					        					
	        				}
	        				selected.clear();
	        			}else if(comboBox.getSelectedItem().equals("Source/Sink")){
	        				//renderer.getEnableState()
	        				int len=comboBox.getItemCount();//.getSelStateList().length;
	        				for(int i=0;i<len;i++){
	        					if(comboBox!=null && comboBox.getItemAt(i)!=null && !comboBox.getItemAt(i).toString().contains("Source/Sink")){
	        						renderer.getSelStateList()[i]=false;
		        					renderer.getEnableState()[i]=false;
	        					}
	        					
	        				}
	        				selected.clear();
	        				//selected.add("Source/Sink");
	        			}
	        			if(renderer.getEnableState()[index]){
	        				
	        				selected.add(comboBox.getSelectedItem().toString());	        			
		        			renderer.getSelStateList()[index]=true;
	        			}
	        			
	        			
	        		}
        		}
        }
        });
       /* comboBox.addKeyListener(new KeyAdapter(){
        	public void keyPressed(KeyEvent e){
        		if(e.getSource() instanceof CustomMultiSelectComboBox){
        			if(e.getKeyCode()==KeyEvent.VK_ENTER){        				
        				CustomMultiSelectComboBox mcb=(CustomMultiSelectComboBox)e.getSource();        				
        				mcb.setPopupVisible(false); 
        				mcb.getBTip().setVisible(false);
        				mcb.getBTip().setEnabled(false);
        				//mcb.setEnabled(false);
        				
        				
        			}
        		}
        	}
        });*/
	}
	
	public void desellectAll(){
		
		//if(comboBox.getSelectedItem().toString()=="Source/Sink"){}
		for(int i=0;i<renderer.getSelStateList().length;i++){
			if(!renderer.getEnableState()[i]){
				renderer.getEnableState()[i]=true;
			}
			renderer.getSelStateList()[i]=false;
		}
		renderer.unselectAll();
		
	}

	public CustomMultiSelectComboBox getMultiSelectComboBox(){
		return comboBox;
	}
	public ArrayList<String> getSelected() {
		selected.clear();
		for(int i=0;i<renderer.getSelectedObjects().size();i++){
			if(renderer.getSelectedObjects().get(i)!=null){
				selected.add(renderer.getSelectedObjects().get(i).toString());
			}
			
		}
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

	public boolean isGraphEditor() {
		return graphEditor;
	}

	public void setGraphEditor(boolean graphEditor) {
		this.graphEditor = graphEditor;
	}
	
	
	
	
	
	
	
	/*public static void main(String[] args){
		 
		JMenuBar menubar = new JMenuBar();
	        JMenu menu = new JMenu();
	        menubar.add(menu);
	        String[] s=new String[]{"a","b","c","d","e","fgsjfgsjgfsjgDRAGANA","g","h"};
	        CustomComboBox myComboBox=new CustomComboBox(s);
	        JFrame frame = new JFrame("My ComboBox");
	        frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
	        frame.setSize(150, 60);
	        frame.setLocation(300, 300);
	        frame.setJMenuBar( menubar );
	        CustomMultiSelectComboBox msb=myComboBox.getMultiSelectComboBox();
	        frame.getContentPane().add(msb);
	        frame.setVisible(true);
	}*/
	
	
	
}
