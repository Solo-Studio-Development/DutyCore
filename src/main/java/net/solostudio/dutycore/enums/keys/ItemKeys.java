package net.solostudio.dutycore.enums.keys;

import net.solostudio.dutycore.item.ItemFactory;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;

public enum ItemKeys {
    FILLER_GLASS_ITEM("filler-glass-item");

    private final String path;

    ItemKeys(@NotNull final String path) {
        this.path = path;
    }

    public ItemStack getItem() {
        return ItemFactory.createItemFromString(path);
    };
}
