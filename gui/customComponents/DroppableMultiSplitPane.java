package fr.inria.contraintes.biocham.customComponents;

import org.jdesktop.swingx.MultiSplitPane;
import fr.inria.contraintes.biocham.plotting.PlotsComparizonWindowUtils;
import java.awt.Cursor;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetContext;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;




/**
 * Class thats creates a custom MultiSplitPane component that supports drag&drop operations over DraggablePanel objects.
 * Used in PlotsComparizonWindowUtils.java
 * 
 * @author Dragana Jovanovska  
 */ 

public class DroppableMultiSplitPane extends MultiSplitPane{

	
	private static DataFlavor dragAndDropPanelDataFlavor = null;
	private final List<DraggablePanel> panels;
	
	public DroppableMultiSplitPane(){
	
		
		super();	
		
		panels = new ArrayList<DraggablePanel>();
	    setDropTarget(new DropTarget(this, new DropPanelTargetListener(this)));
	}

	
	
	
	
	public static DataFlavor getDragAndDropPanelDataFlavor() throws Exception {
		// Lazy load/create the flavor
		if (dragAndDropPanelDataFlavor == null) {
			dragAndDropPanelDataFlavor = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType + ";class=java.lang.Object","DraggablePanel");
		}
		
		return dragAndDropPanelDataFlavor;
	}
	
	 protected void relayout() {
		 PlotsComparizonWindowUtils.updateComparisonWindow();
	 }
	 
	
	 
	 /**
	     * <p>Returns the List of user-added panels.</p>
	     * <p>Note that for drag and drop, these will be cleared, and the panels will be added back in the correct order!</p>
	     * @return
	     */
	 public List<DraggablePanel> getDragAndDropPanels() {
		 return panels;
	 }
		
}


/**
 * <p>Listens for drops and performs the updates.</p>
 * <p>The real magic behind the drop!</p>
 */
class DropPanelTargetListener implements DropTargetListener {

    private final DroppableMultiSplitPane rootPanel;
    
    /**
     * <p>Two cursors with which we are primarily interested while dragging:</p>
     * <ul>
     *   <li>Cursor for droppable condition</li>
     *   <li>Cursor for non-droppable consition</li>
     * </ul>
     * <p>After drop, we manually change the cursor back to default, though does this anyhow -- just to be complete.</p>
     */
    private static final Cursor droppableCursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR),
            notDroppableCursor = Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR);

    public DropPanelTargetListener(DroppableMultiSplitPane sheet) {
        this.rootPanel = sheet;
    }

    // Could easily find uses for these, like cursor changes, etc.
    public void dragEnter(DropTargetDragEvent dtde) {}
    public void dragOver(DropTargetDragEvent dtde) {
        if (!this.rootPanel.getCursor().equals(droppableCursor)) {
            this.rootPanel.setCursor(droppableCursor);
        }
    }
    public void dropActionChanged(DropTargetDragEvent dtde) {}
    public void dragExit(DropTargetEvent dte) {
        this.rootPanel.setCursor(notDroppableCursor);
    }

    /**
     * <p>The user drops the item. Performs the drag and drop calculations and layout.</p>
     * @param dtde
     */
    public void drop(DropTargetDropEvent dtde) {
        
        //System.out.println("Step 5 of 7: The user dropped the panel. The drop(...) method will compare the drops location with other panels and reorder the panels accordingly.");
        
        // Done with cursors, dropping
        this.rootPanel.setCursor(Cursor.getDefaultCursor());
        
        // Just going to grab the expected DataFlavor to make sure
        // we know what is being dropped
        DataFlavor dragAndDropPanelFlavor = null;
        
        Object transferableObj = null;
        Transferable transferable = null;
        
        try {
            // Grab expected flavor
            dragAndDropPanelFlavor = DroppableMultiSplitPane.getDragAndDropPanelDataFlavor();
            
            transferable = dtde.getTransferable();
            DropTargetContext c = dtde.getDropTargetContext();
           
            // What does the Transferable support
            if (transferable.isDataFlavorSupported(dragAndDropPanelFlavor)) {
                transferableObj = dtde.getTransferable().getTransferData(dragAndDropPanelFlavor);
            } 
            
        } catch (Exception ex) { /* nope, not the place */ }
        
        // If didn't find an item, bail
        if (transferableObj == null) {
            return;
        }
        
        // Cast it to the panel. By this point, we have verified it is 
        // a RandomDragAndDropPanel.
        DraggablePanel droppedPanel = (DraggablePanel)transferableObj;
        
        // Get the y offset from the top of the WorkFlowSheetPanel
        // for the drop option (the cursor on the drop)
        final int dropYLoc = dtde.getLocation().y;

        // We need to map the Y axis values of drop as well as other
        // RandomDragAndDropPanel so can sort by location.
        Map<Integer, DraggablePanel> yLocMapForPanels = new HashMap<Integer, DraggablePanel>();
        yLocMapForPanels.put(dropYLoc, droppedPanel);

        // Iterate through the existing demo panels. Going to find their locations.
        for (DraggablePanel nextPanel : rootPanel.getDragAndDropPanels()) {

            // Grab the y value
            int y = nextPanel.getY();

            // If the dropped panel, skip
            if (!nextPanel.equals(droppedPanel)) {
                yLocMapForPanels.put(y, nextPanel);
            }
        }

        // Grab the Y values and sort them
        List<Integer> sortableYValues = new ArrayList<Integer>();
        sortableYValues.addAll(yLocMapForPanels.keySet());
        Collections.sort(sortableYValues);

        // Put the panels in list in order of appearance
        List<DraggablePanel> orderedPanels = new ArrayList<DraggablePanel>();
        for (Integer i : sortableYValues) {
            orderedPanels.add(yLocMapForPanels.get(i));
        }
        
        // Grab the in-memory list and re-add panels in order.
        List<DraggablePanel> inMemoryPanelList = this.rootPanel.getDragAndDropPanels();
        inMemoryPanelList.clear();
        inMemoryPanelList.addAll(orderedPanels);
    
        // Request relayout contents, or else won't update GUI following drop.
        // Will add back in the order to which we just sorted
        this.rootPanel.relayout();
    }
} // DemoPanelDropTargetListener