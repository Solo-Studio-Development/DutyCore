package net.solostudio.dutycore.menu;

import net.solostudio.dutycore.managers.MenuController;
import net.solostudio.dutycore.processor.MessageProcessor;
import org.bukkit.Bukkit;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.InventoryHolder;
import org.jetbrains.annotations.NotNull;

public abstract class Menu implements InventoryHolder {

    protected MenuController menuController;
    protected Inventory inventory;

    public Menu(@NotNull MenuController menuController) {
        this.menuController = menuController;
    }

    public abstract void handleMenu(final InventoryClickEvent event);
    public abstract void setMenuItems();

    public abstract String getMenuName();

    public abstract int getSlots();
    public abstract int getMenuTick();

    @Override
    public @NotNull Inventory getInventory() {
        return inventory;
    }

    public void open() {
        inventory = Bukkit.createInventory(this, getSlots(), MessageProcessor.process(getMenuName()));

        this.setMenuItems();

        menuController.owner().openInventory(inventory);
        MenuUpdater menuUpdater = new MenuUpdater(this);
        menuUpdater.start(getMenuTick());
    }

    public void close() {
        MenuUpdater menuUpdater = new MenuUpdater(this);

        menuUpdater.stop();

        inventory = null;
    }

    public void updateMenuItems() {
        if (inventory == null) return;

        setMenuItems();
        menuController.owner().updateInventory();
    }
}