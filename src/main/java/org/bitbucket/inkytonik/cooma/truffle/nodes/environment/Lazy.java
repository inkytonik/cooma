package org.bitbucket.inkytonik.cooma.truffle.nodes.environment;

import lombok.RequiredArgsConstructor;

import java.util.function.Supplier;

//https://dzone.com/articles/leveraging-lambda-expressions-for-lazy-evaluation
@RequiredArgsConstructor
public class Lazy<T> {

    private final Supplier<T> supplier;

    private volatile T value;

    public T get() {
        if (value == null) {
            synchronized (this) {
                if (value == null) {
                    value = supplier.get();
                }
            }
        }
        return value;
    }

    public void setValue(T value) {
        this.value = value;
    }

    public static <T> Lazy<T> of(Supplier<T> supplier) {
        return new Lazy<>(supplier);
    }

}