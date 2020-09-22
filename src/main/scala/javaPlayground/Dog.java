package javaPlayground;

public class Dog {
    public String getName() {
        return name;
    }

    public Dog() {
    }

    public Dog(String s) {
        this.name = s;
    }

    public void setName(String name) {
        this.name = name;
    }

    private String name;

    @Override
    public String toString() {
        return "Dog{" +
                "name='" + name + '\'' +
                '}';
    }
}
