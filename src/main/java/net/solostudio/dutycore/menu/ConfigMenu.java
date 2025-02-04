package net.solostudio.dutycore.menu;

import net.solostudio.dutycore.item.ItemFactory;
import net.solostudio.dutycore.managers.MenuController;
import net.solostudio.dutycore.processor.MessageProcessor;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.inventory.InventoryType;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Optional;
import java.util.stream.IntStream;

public class ConfigMenu extends Menu {
    private final ConfigurationSection config;

    public ConfigMenu(@NotNull MenuController menuController, ConfigurationSection config) {
        super(menuController);
        this.config = config;
    }

    @Override
    public void handleMenu(final InventoryClickEvent event) {
        event.setCancelled(true);

        var slot = event.getSlot();
        Optional.ofNullable(config.getConfigurationSection("items"))
                .map(items -> items.getConfigurationSection(String.valueOf(slot)))
                .map(itemConfig -> itemConfig.getConfigurationSection("action"))
                .ifPresent(actionSection -> handleAction(event, actionSection));
    }

    private void handleAction(InventoryClickEvent event, ConfigurationSection actionSection) {
        var player = (Player) event.getWhoClicked();
        var clickType = event.getClick();

        var actionTypes = actionSection.getStringList("type");
        var actions = actionSection.getStringList("do");

        if (actionTypes.stream().anyMatch(type -> type.equalsIgnoreCase("[" + clickType.name() + "]"))) {
            for (String action : actions) {
                if (action.startsWith("[MESSAGE]")) {
                    player.sendMessage(action.replace("[MESSAGE]", "").trim());
                } else if (action.startsWith("[COMMAND]")) {
                    Bukkit.dispatchCommand(Bukkit.getConsoleSender(),
                            action.replace("[COMMAND]", "").replace("%player%", player.getName()).trim());
                }
            }
        }
    }

    @Override
    public void setMenuItems() {
        Optional.ofNullable(config.getConfigurationSection("items"))
                .ifPresent(itemsSection -> itemsSection.getKeys(false).forEach(key -> {
                    var slot = Integer.parseInt(key);
                    var itemConfig = itemsSection.getConfigurationSection(key);

                    if (itemConfig != null) {
                        Material material = Material.valueOf(itemConfig.getString("material", "STONE"));
                        int amount = itemConfig.getInt("amount", 1);
                        String name = MessageProcessor.process(itemConfig.getString("name", ""));
                        List<String> lore = itemConfig.getStringList("lore");

                        ItemStack item = ItemFactory.create(material, amount)
                                .setName(name)
                                .setLore(lore.toArray(new String[0]))
                                .finish();

                        inventory.setItem(slot, item);
                    }
                }));
    }


    @Override
    public String getMenuName() {
        return config.getString("title", "Menu");
    }

    @Override
    public int getSlots() {
        var sizeConfig = config.getString("size", "9");
        try {
            return Integer.parseInt(sizeConfig);
        } catch (NumberFormatException exception) {
            try {
                InventoryType type = InventoryType.valueOf(sizeConfig.toUpperCase());
                return Bukkit.createInventory(menuController.owner(), type).getSize();
            } catch (IllegalArgumentException ignored) {
                return 9;
            }
        }
    }

    @Override
    public int getMenuTick() {
        return config.getInt("update-tick", 20);
    }
}
